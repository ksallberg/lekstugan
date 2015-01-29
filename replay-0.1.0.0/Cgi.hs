module Cgi (

   -- | Types
     Cgi
   , Type (..)
   , Id
   , Question
   , Answer

   -- | Operations
   , pickStr
   , pickInt
   , ask
   , liftIO

   -- | Run function
   , runCGI

) where

import Data.List.Split
import Data.List
import Data.Char
import Replay
import Text.Html
import Network.CGI.Protocol

type Cgi a = Replay Question Answer a
-- | We use Type to differeniate between
--   the types a user can enter into a
--   textfield. We currently support "int"
--   and "str" when "type checking"

-- This is the options developers can use to define
-- what type of input should be entered.
data Type = IsString | IsInt
   deriving (Eq,Show,Read,Ord)

type Id = Int
-- | Question and Answer are the same type.
--   The Id is unique for each question
--   and is used later on to get the result.
--   The answer itself is a String that might
--   be able to be read as an Int if the Type
--   is "int". Not if the Type is "str".
type Question = [(Id, String, Type)]
type Answer = Question

-- | Pick a trace out of the Haskell reads
--   function without risking that the empty
--   list causes an exception
safeTrace :: [(Trace a,String)] -> Trace a
safeTrace []        = emptyTrace
safeTrace ((x,y):_) = x

-- | Helper method putting the contents of a list
--   into pairs (still in a list)
pair (x:y:xs) = (x,y) : pair xs
pair _ = []

-- | Get just the head out of Haskell's reads
--   (the actual parsed value)
reads' :: String -> (Id,Type)
reads' x = (i, read t :: Type)
           where (i,t) = head (reads x :: [(Id,String)])

-- | Used to add many answers recursively through
--   the addAnswer function imported from Replay
--   type :: [(i,q,t)]
addAnswers :: Trace Answer -> Answer -> Trace Answer
addAnswers t a | length a > 0 = t `addAnswer` a
               | otherwise    = t

-- | Check that an answer is of the type
--   it's supposed to be. Supports "str" and "int"
correctTypes :: Answer -> Bool
correctTypes []     = True
correctTypes ((_,a,t):xs) =
    case t of
        IsString -> True && correctTypes xs
        IsInt    -> and [isNumber x | x <- a] && correctTypes xs

-- | Run function for the CGI monad
--   The trace is stored in one single hidden tag
--   in the web page. That means we have to parse it when
--   receiving it back from the web server.
runCGI :: Cgi () -> IO ()
runCGI rep = do

  s <- getContents  -- gets all input from stdi

  -- Divide the POST into answers, and the trace
  let answersAndTrace = splitOn "&trace=" (urlDecode s)
      -- Get the trace written to HTML in the last run
      trace = safeTrace
               (reads (
                  urlDecode
                     (last answersAndTrace)) :: [(Trace Answer,String)])
      -- Create a handle for just the answers
      answers = init answersAndTrace

      -- Splitting answers into [(question,answer)]
      answers' = pair $ concat [splitOn "=" a
                               | a <- splitOn "&" (concat answers)
                               ]

      -- Apply the haskell parsing (reads) to questions
      -- This is because we store lists and tuples with the show
      -- function generating strings looking exactly like Haskell code
      answers'' = map (\(it,a) -> (fst $reads' it, a, snd $reads' it))
                      answers'

      -- check if the answers are correct or not
      typeSafe = correctTypes answers''

      -- Create a new trace, but don't do it if the answers
      -- received are not type safe ("int" and "str")
      newTrace = case typeSafe of
                    True  -> trace `addAnswers` answers''
                    False -> trace

  -- put the page headers here
  putStr (unlines pageInit)

  -- "call" the monad to get whatever page to show next
  -- because of the newTrace call above, we just repeat
  -- the questions if a type check error was detected
  --
  -- If not, give the next page of questions
  nextRun <- (runReplay rep) newTrace newTrace

  -- If the final page is to be displayed, just show it (Right)
  case nextRun of
    (_,Right (a,r)) -> putStr ( unlines ( genFinalPage ) )
    -- Otherwise, Show the same page with error notations
    -- if a type error was detected, otherwise show the next page
    (r,Left qs) ->
      case typeSafe of
         True ->
            putStr ( unlines (genNormalPage qs r ))
         False ->
            putStr ( unlines (genErrorPage qs r))

-- | Pick the string (answer value) from an answer
pickStr :: Id -> Answer -> String
pickStr seek answerT =
   let (a,b,c) = head (dropWhile (\(x,y,z)->x < seek) (sort answerT))
   in b

-- | Pick the answer value and turn it to an int
--   If the answer is \"\", just return 0
pickInt :: Id -> Answer -> Int
pickInt seek answerT = case pickStr seek answerT of
                          "" -> 0
                          str  -> read str :: Int

-- | Headers for the browser to read
pageInit :: [String]
pageInit = [ "Content-type: text/html"
           , "" -- This newline is significant.
           , "<html><body>"
           ]

-- |????????
genFinalPage :: [String]
genFinalPage =
 [
   "</body></html>"
 ]

-- | The error page is different from the normal page in that
--   it tells the user to fill in
genErrorPage :: Answer -> Trace Answer -> [String]
genErrorPage qs r =
 [
  "<form method=POST>"
 , "<p>"
 , "<br/>"
 , concat [q ++ "<input name=" ++ show i ++ show t ++
           "> "++"<font color=red>"++formatError t++"</font>"++"<br/>"
          | (i,q,t) <- qs
          ]
       , "<input type=submit value=OK>"
       , "<input type=hidden name=trace value="++urlEncode (show r)++" />"
       , "</p>"
       , "</form>"
       , "</body></html>"
 ]

-- | Replace the "str" and "int" errors with
--   a more human friendly error message
formatError :: Type -> String
formatError IsString = ""
formatError IsInt    = "Please enter an integer."

-- | Show the amount of question text boxes needed
--   for the current step in the form
genNormalPage :: Answer -> Trace Answer -> [String]
genNormalPage qs r =
 [
  "<form method=POST>"
 , "<p>"
 , "<br/>"
 , concat [q ++ "<input name=" ++ show i ++ show t ++ "><br/>" | (i,q,t) <- qs ]
 , "<input type=submit value=OK>"
 , "<input type=hidden name=trace value="++urlEncode (show r)++" />"
 , "</p>"
 , "</form>"
 , "</body></html>"
 ]
