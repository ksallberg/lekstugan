{-
     name: Kristian Sällberg
     matriculation number: 2057128s
     course: Functional Programming 4
     excercise title: Calculator
     date: 29 November 2012
-}

{-   A basic calculator.  To compiler and run, enter these commands:

     * This does not work for me on OSX
     with my installation of Gtk:
      ghc --make ExtendedCalc
     ./ExtendedCalc
     
     What works for me is running "runhaskell ExtendedCalc"

     What I attempted:
     all 7 standard solutions

     +

     and: 8,  concurrency
          11, financial calculations
          12, graphs of math functions
          ?,  implemented fix so that only 
              valid Double's can be entered to the stack
          1,  GUI refinements:
                 + showed the stack with one value per line
                 + added a text box that shows the current
                   state of the memory cell

     Other people's code I used (or was inspired by):

     I used an example from here, to implement
     the drawing of mathematical graphics in Cairo
     http://www.cse.chalmers.se/edu/year/2010/course/
     TDA451_Functional_Programming/labs/4/gtk2hs.html
-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Financial
import DrawGraphics
import Data.String

{- This data structure holds the state of the system. -}

data CalcState = CalcState
  { displayString :: String      -- what's displayed at the top
  , stack         :: [Double]    -- stack of numbers
  , dispEntry     :: Entry       -- the gtk Entry for the display
  , stackBuf      :: TextBuffer  -- the gtk TextBuffer for the stack
  , memCellBuf    :: TextBuffer  -- textbuffer to display what's in memCell
  , memCell       :: Double      -- the memory cell for STO and FET
  , threads       :: [ThreadId]  -- collect the threads
  }

{- A single state reference sr :: SR is created, which always points
to the current state of the system. -}

type SR = IORef CalcState

{- Initial state of the calculator.  The dispEntry and stackBuf fields
need to be updated with the actual gtk values, which are created
dynamically when the program is initialising itself.  The error values
given in the initState enable the program to crash with an informative
message if these fields are not initialised properly before they are
used. -}

initState :: CalcState
initState         = CalcState
  { displayString = ""
  , stack         = []
  , dispEntry     = error "Display entry not set"
  , stackBuf      = error "Stack text buffer not set"
  , memCellBuf    = error "Memory cell buffer not set"
  , memCell       = error "Memcell not set yet"
  , threads       = []
  }

{- The main program initialises the widgets and then starts the
GUI. -}

main :: IO ()
main =
  do initGUI
     timeoutAddFull
       (yield >> return True)
       priorityDefaultIdle 50

-- Read in the glade file

     let gladeFilePath = "glade/calculator.glade"
     maybe_xml <- xmlNew gladeFilePath
     let xml = case maybe_xml of
           Just xml_text -> xml_text
           Nothing -> error "cannot open glade xml file"

-- Set up the main window

     mainWindow <- xmlGetWidget xml castToWindow "MainWindow"
     onDestroy mainWindow mainQuit

-- Initialise the state reference

     sr <- newIORef initState

-- Activate Menu: File: Quit

     quitMenuAction <- xmlGetWidget xml castToMenuItem "menuQuit"
     onActivateLeaf quitMenuAction $ do mainQuit

-- Initialise the display entry (the top field for entering numbers)

     displayEntry <- xmlGetWidget xml castToEntry "DisplayEntry"
     s <- readIORef sr
     writeIORef sr (s {dispEntry = displayEntry})

-- Initialise the stack view (the text view at position 3)

     stackView <- xmlGetWidget xml castToTextView "StackTextView"
     textBufTagTable <- textTagTableNew
     stackTextBuf <- textBufferNew (Just textBufTagTable)
     textViewSetBuffer stackView stackTextBuf
     textBufferSetText stackTextBuf ""
     s <- readIORef sr
     writeIORef sr (s {stackBuf = stackTextBuf})

-- initialise the memCell view
     memCellView <- xmlGetWidget xml castToTextView "MemCellView"
     memTextBufTagTable <- textTagTableNew
     memCellTextBuf <- textBufferNew (Just memTextBufTagTable)
     textViewSetBuffer memCellView memCellTextBuf
     textBufferSetText memCellTextBuf "memory cell: "
     s <- readIORef sr
     writeIORef sr (s {memCellBuf = memCellTextBuf})

-- Set up the digit and decimal point buttons

     forM
       [("b0",'0'), ("b1",'1'), ("b2",'2'), ("b3",'3'), ("b4",'4'),
        ("b5",'5'), ("b6",'6'), ("b7",'7'), ("b8",'8'), ("b9",'9'),
        ("bpoint",'.')]
       (\(x,y) -> prepareNumButton sr xml x y)

{- Set up the Enter button
   modification: Checking to see that the value entered
   is actually a Double value (discard 1., ., .1 etc) -}
     benter <- xmlGetWidget xml castToButton "benter"
     onClicked benter $ do
       s <- readIORef sr
       let toCheck = displayString s
       test <- try $ print (read toCheck :: Double) :: IO (Either SomeException ())
       case test of
          Left e  -> putStrLn "Error, entered value is not a valid Double!"
          Right n -> do setStack sr ((read (displayString s) :: Double) : stack s)
                        setDisplay sr ""

-- Clear the display (Standard Imp 1)
     ceButton <- xmlGetWidget xml castToButton "bCE"
     onClicked ceButton $ do
       setDisplay sr ""

-- Clear the display AND the stack (Std Imp 2)
     clrButton <- xmlGetWidget xml castToButton "bCLR"
     onClicked clrButton $ do
       setStack   sr []
       setDisplay sr ""

-- Event listener for the +/- button (Std Imp 3)
     prepareUnopButton sr xml "bChangeSign" negate

-- Event listeners for sqrt, sin & cos (Std Imp 4)
     --sqrt
     prepareUnopButton sr xml "bSqrt" sqrt

     --sin
     prepareUnopButton sr xml "bSin" sin

     --cos
     prepareUnopButton sr xml "bCos" cos

-- Event listener for STO (Std Imp 5)
     stoButton <- xmlGetWidget xml castToButton "bSto"
     onClicked stoButton $ do
       s <- readIORef sr
       if length (stack s) == 0
         then return ()
         else do let headStack = show $ head (stack s)
                 writeIORef sr (s {memCell = head (stack s)})
                 setStack sr $ tail (stack s)
                 textBufferSetText (memCellBuf s) $ "memory cell: " 
                                                    ++ headStack

     --FET
     fetButton <- xmlGetWidget xml castToButton "bFet"
     onClicked fetButton $ do
       s <- readIORef sr
       setStack sr $ (memCell s):(stack s)

-- Event listener for EXCH (Std Imp 6)
     exchButton <- xmlGetWidget xml castToButton "bEXCH"
     onClicked exchButton $ do
       s <- readIORef sr
       if length (stack s) < 2
          then return ()
          else do let fstTop = head (stack s)
                  let sndTop = head (tail $ stack s)
                  setStack sr $ sndTop:fstTop:(drop 2 (stack s))

-- Event listener for "about" (Std Imp 7)
     aboutWindow <- xmlGetWidget xml castToWindow "AboutWindow"

     aboutMenuAction <- xmlGetWidget xml castToMenuItem "menuAbout"
     onActivateLeaf aboutMenuAction $ do
       widgetShowAll aboutWindow
       return ()

{- Event Listener for "present value" (financial)
   Assumes the first member of the stack is the percentage,
   and all the following are cash flow per annum
-}
     pvButton <- xmlGetWidget xml castToButton "bPV"
     onClicked pvButton $ do
       s <- readIORef sr
       if length (stack s) >= 2
         then do 
           let res = presentValue (tail (stack s)) (head (stack s))
           setStack sr [res]
           setDisplay sr (show res)
         else return ()

{- Calculate Net Present Value (financial)
   Assume the stack looks like:
   initial rf b cashflow salvage years
-}
     npvButton <- xmlGetWidget xml castToButton "bNPV"
     onClicked npvButton $ do
       s <- readIORef sr
       if length (stack s) /= 6
          then return()
          else do let initial  = (stack s) !! 0
                  let b        = (stack s) !! 1
                  let riskfree = (stack s) !! 2
                  let cashflow = (stack s) !! 3
                  let salvage  = (stack s) !! 4
                  let years    = (stack s) !! 5
                  let res      = npv initial  b
                                     riskfree cashflow
                                     salvage  years
                  setStack sr [res]
                  setDisplay sr (show res)

     -- event listeners for the concurrent slow calculations
     slowButton <- xmlGetWidget xml castToButton "bSLOW1"
     onClicked slowButton  $ initSlowUseless sr "slow1" 100000

     slow2Button <- xmlGetWidget xml castToButton "bSLOW2"
     onClicked slow2Button $ initSlowUseless sr "slow2" 100000
     
     stopButton <- xmlGetWidget xml castToButton "bSTOP"
     onClicked stopButton $ stopSlowUseless sr

     -- event listeners for the draw graphs of mathematical funcs
     drawSinButton <- xmlGetWidget xml castToButton "bDRAWSIN"
     onClicked drawSinButton $ openGraph sr sin "Sin Window"

     drawCosButton <- xmlGetWidget xml castToButton "bDRAWCOS"
     onClicked drawCosButton $ openGraph sr cos "Cos Window"

     drawTanButton <- xmlGetWidget xml castToButton "bDRAWTAN"
     onClicked drawTanButton $ openGraph sr tan "Tan Window"

-- Set up the operator buttons
     prepareBinopButton sr xml "bAdd" (+)
     prepareBinopButton sr xml "bSub" (-)
     prepareBinopButton sr xml "bMul" (*)
     prepareBinopButton sr xml "bDiv" (/)
     prepareUnopButton  sr xml "bReciprocal" (1/)

-- Start up the GUI
     
     widgetShowAll mainWindow
     mainGUI

-- Advanced extension 12: Mathematical functions in Cairo
openGraph :: SR -> (Double -> Double) -> String -> IO ()
openGraph sr fun title = 
   do s <- readIORef sr
      if length (stack s) < 3
         then return ()
         else do
            let waveLen   = (stack s) !! 0
            let rangeFrom = (stack s) !! 1
            let rangeTo   = (stack s) !! 2
            openGraphWindow fun (rangeFrom,rangeTo) waveLen title

-- Advanced extension 8: Concurrency

{- Initialize a potentially slow and blocking
   (if I wasn't using concurrency) calculation
-}
initSlowUseless :: SR -> String -> Int -> IO ()
initSlowUseless sr str limit
   = do s <- readIORef sr
        -- give the new thread a reference to it's own id
        newThread <- performUseless str limit sr
        writeIORef sr (s {threads = newThread : (threads s) })

{- Stop all running slow calculations! -}
stopSlowUseless :: SR -> IO ()
stopSlowUseless sr
   = do s <- readIORef sr
        killManyThreads (threads s) --kill all the running threads
        writeIORef sr (s {threads = []} )

{- Removes a thread from the current state
   where it is stored (this is used when
   a long computation is finished)
-}
removeThread :: SR -> IO ()
removeThread sr =
   do s <- readIORef sr
      curId <- myThreadId
      writeIORef sr (s {threads = ([x| x <-(threads s), x /= curId])})

{- Kill all threads of a list on demand!
-}
killManyThreads :: [ThreadId] -> IO ()
killManyThreads []     = return ()
killManyThreads (x:xs) = do killThread x
                            killManyThreads xs

{- Just fork up a new thread doing something useless
-}
performUseless :: String -> Int -> SR -> IO ThreadId
performUseless msg upperLimit sr = forkIO (rep msg 0 upperLimit sr)

{- Do something useless, just repeatedly 
   print things to the terminal
-}
rep :: String -> Int -> Int -> SR -> IO()
rep str counter upperLimit sr
   | counter == upperLimit = removeThread sr -- when finished, 
                                             -- remove yourself
   | otherwise = do putStrLn $ str ++ (show counter)
                    rep str (counter+1) upperLimit sr

{- Set the stack to xs.  The new stack is shown in the text view on
the GUI, and is also printed to the console. -}

setStack :: SR -> [Double] -> IO ()
setStack sr xs =
  do s <- readIORef sr
     let str = show xs
     let newLinesInserted = lsReplace ',' '\n' str
     textBufferSetText (stackBuf s) newLinesInserted
     putStrLn ("Stack: " ++ str)
     writeIORef sr (s {stack = xs})


{- Set the display to xs.  This is set in the GUI, and also printed on
the console. -}

setDisplay :: SR -> String -> IO ()
setDisplay sr xs =
  do s <- readIORef sr
     entrySetText (dispEntry s) xs
     writeIORef sr (s {displayString = xs})
     putStrLn xs

{- This function takes several parameters needed to describe an
operator with two operands, such as + or *, and it sets up the
button. -}

prepareBinopButton
  :: SR -> GladeXML -> String -> (Double -> Double -> Double) -> IO ()
prepareBinopButton sr xml bname f =
  do button <- xmlGetWidget xml castToButton bname
     onClicked button $ do
       s <- readIORef sr
       case stack s of
         x:y:stack' ->
           do let r = f x y
              setStack sr (r:stack')
              setDisplay sr (show r)
         _ -> return ()
     return ()

{- This function is similar to prepareBinopButton, but it's for
operators that take only one argument. -}

prepareUnopButton
  :: SR -> GladeXML -> String -> (Double -> Double) -> IO ()
prepareUnopButton sr xml bname f =
  do button <- xmlGetWidget xml castToButton bname
     onClicked button $ do
       s <- readIORef sr
       case stack s of
         x:stack' ->
           do let r = f x
              setStack sr (r:stack')
              setDisplay sr (show r)
         _ -> return ()
     return ()

{- This function sets up a button that is used to enter data into the
display, in particular digits and the decimal point. -}

prepareNumButton :: SR -> GladeXML -> String -> Char -> IO ()
prepareNumButton sr xml bname bchar =
  do button <- xmlGetWidget xml castToButton bname
     onClicked button $ do
       s <- readIORef sr
       let newstr = displayString s ++ [bchar]
       setDisplay sr newstr
     return ()

lsReplace :: Eq a => a -> a -> [a] -> [a]
lsReplace _ _ [] = []
lsReplace lookFor replaceBy (x:xs)
   = if x == lookFor
        then replaceBy : lsReplace lookFor replaceBy xs
        else x         : lsReplace lookFor replaceBy xs
