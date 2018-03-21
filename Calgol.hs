{-
   Identification information:

   name: Kristian Sällberg
   matriculation number: 2057128S
   course: Functional Programming 4
   exercise title: Programming Language Interpreter
   date: tuesday 30th October
-}

{-
   Status report:

   I have implemented the basic parser and the basic interpreter, they
   work in the sense that I've not been able to find any bugs

   I have not implemented any of the extra features.

   Known bugs:
      In the interpreter, it's not possible to assign constants. This is more
      of a "not implemented" problem than a bug though

   the function 'program "file.calgol"' can be used to parse from a file

   programFromText can be used to pass "calgol" source code straight
   to the function without storing it in a file.

   the main function just uses programFromText so that I can give a demo
   of the parser and the interpreter within this haskell file.
-}

module Calgol where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Monad.State

type Var = String

{-
   Just a simple test
-}
main :: IO ()
main = programFromText $
        unlines [ "{"
                , "[a,b,c,n,i,apa,bepa]"
                , "         read i;"
                , "         read a;"
                , "         n := 1;"
                , "         while (i>0) {"
                , "           n := 2*n;"
                , "           i := i-1;"
                , "           if(apa==bepa) {"
                , "             read apa;"
                , "             while(a==b) {"
                , "               write apa;"
                , "             };"
                , "           } else {"
                , "             write bepa;"
                , "           };"
                , "         };"
                , "         write n;"
                , "         i := 2*3;"
                , "         while(i==3) {"
                , "           [w]"
                , "           if(a==2){"
                , "             i:=n;"
                , "           }else{"
                , "             i:=2;"
                , "           };"
                , "        };"
                , "};" ]

{-
   Language name: calgol

   := is for variable assignment
   = is for constant assignment
   == is for comparison
-}
data IntExp
   = IVar Var
   | ICon Int
   | Add IntExp IntExp
   | Sub IntExp IntExp
   | Mul IntExp IntExp
   | Div IntExp IntExp
   deriving (Show,Read)

data BoolExp
   = LThan IntExp IntExp
   | Equals IntExp IntExp
   | GThan IntExp IntExp
   deriving (Show,Read)

{- I have replace begin with block throught the program
   to reflect my C style syntax
-}
data Stmt
   = Block [Decl] [Stmt]
   | AssignVar Var IntExp
   | AssignCon Var IntExp
   | Read Var
   | Write IntExp
   | IfThenElse BoolExp Stmt Stmt
   | While BoolExp Stmt
   deriving (Show,Read)

type Decl = Var -- for now, a decl is just a variable

{-
   Just avoiding magic variables in the code
-}
supportedOps :: String
supportedOps = "+-*/"

{-
   Designed to parse a variable name. In my language,
   variable names can not start with a digit

   run parseVar "minLength =2" => "minLength"
   run parseVar "23" => fail
   run parseVar "2minLength" => fail
-}
parseVar :: Parser Var
parseVar = 
   do spaces
      first <- letter
      varName <- many alphaNum
      spaces
      return (first:varName)

{- parsers for the IntExp constructor

   run parseIntExp "2*3" => Mul 2 3
   run parseIntExp "2*4+1" => Mul 2 (Add 3 1) --wrong op prec
   run parseIntExp "ab-23" => Sub ab 32
   run parseIntExp "ddd" => dd (IVar dd)
   run parseIntExp "23" => 23 (ICon 23)
   run parseIntExp "" => fail (no input!)
-}
parseIntExp :: Parser IntExp
parseIntExp = try parseBinOps <|> parseVarOrConst

{-
   This uses parseVar, and just wraps it in a IVar
   so not gonna unit test it.
-}
parseIVar :: Parser IntExp
parseIVar =
   do varName <- parseVar
      return (IVar varName)

{-
   run parseConstant "23" => should work
   run parseConstant "23a" => should fail, a not allowed
   run parseConstant "23+" => should fail, op not allowed
   run parseConctant "a" => should fail
-}
parseConstant :: Parser IntExp
parseConstant =
   do spaces
      constant <- many1 digit
      spaces
      notFollowedBy alphaNum
      return (ICon (read constant::Int))

{-
   Combinator for parsing either a variable name (alphaNum)
   or a contant (just digits)
   as of now, all variable names have to start with a character

   run parseVarOrConst "233" => ICon 233
   run parseVarOrConst "abv" => IVar abc
   run parseVarOrConst "" => fail no input
-}
parseVarOrConst :: Parser IntExp
parseVarOrConst = try parseIVar <|> parseConstant

{-
   Parse any binop
   this can also parse a varOrConst because it's
   co-recursive with parseOps (which needs to be able to parse
   vars or constans)

   run parseBinOps "332+34*23/42"
      => Add (ICon 332) (Mul (ICon 34) (Div (ICon 23) (ICon 42)))
   run parseBinOps "233" => ICon 233
-}
parseBinOps :: Parser IntExp
parseBinOps = try parseOps <|> try parseVarOrConst

{-
   Co recursive with parseBinOps. find one or several operators
   in a row and return them in a recursive datatype
-}
parseOps :: Parser IntExp
parseOps =
   do spaces
      varLeft <- parseVarOrConst
      spaces
      op <- oneOf supportedOps
      spaces
      varRight <- parseBinOps
      return (getMatchingBinOp op varLeft varRight)

{-
   Very simple auxillary function to get the right data type
   back when inserting an operator, based on pattern matching
-}
getMatchingBinOp :: Char -> IntExp -> IntExp -> IntExp
getMatchingBinOp '+' a b = Add a b
getMatchingBinOp '-' a b = Sub a b
getMatchingBinOp '*' a b = Mul a b
getMatchingBinOp '/' a b = Div a b

{-
   Parse boolean expressions

   run parseBool "a>3" => GThan a 3
   run parseBool "a==3" => Equals a 3
   run parseBool "a<3" => LThan a 3
-}
parseBool :: Parser BoolExp
parseBool =
   do spaces
      left <- parseIntExp
      spaces
      op <- string ">" <|> string "<" <|> string "=="
      spaces
      right <- parseIntExp
      spaces
      return (getMatchingBoolOp op left right)

{-
   Simple auxillary function to get the matching data
   type back, when passing an operator
-}
getMatchingBoolOp :: String -> IntExp -> IntExp -> BoolExp
getMatchingBoolOp ">"  a b = GThan  a b
getMatchingBoolOp "<"  a b = LThan  a b
getMatchingBoolOp "==" a b = Equals a b


{-
   This is what can parse a program. It looks for statements ended
   with a semi-colon.

   run parseStmt "{
                   [a,b,c]
                   read i;
                   n := 1;
                   while (i>0) {
                     n := 2*n;
                     i := i-1;
                     if(apa==bepa) {
                       read apa;
                         while(a==b) {
                           write apa;
                         };
                     } else {
                       write bepa;
                     };
                   };
                   write n;
                   i= 2*3;
                   while(p==3) {
                     [w]
                     if(a==2){
                       i:=n;
                     }else{
                       i:=2;
                     };
                   };
                 };" => gives syntax tree back
   run parseStmt "i=2;" => AssignCon "i" (ICon 2)
   run parseStmt "2;" => fail (no stmt)
   run parseStmt "a<2;" => fail (no stmt)
-}
parseStmt :: Parser Stmt
parseStmt =
   do spaces
      stmt <- parseStmtAll
      spaces
      char ';'
      spaces
      return stmt

{-
   This is a collection of all parts that form the statement. It can parse all
   subparts in a stmt. It is used by parseStmt

   run parseStmt "while(b>a){
                    while(y < x){
                      if(r€t) {
                        write x
                      }
                      else {
                        read h
                      }
                    }
                  }"
      => gives syntax tree
-}
parseStmtAll :: Parser Stmt
parseStmtAll = try parseBlock <|>
               try parseWhile <|>
               try parseIfThenElse <|>
               try parseAssign <|>
               try parseWrite <|>
               parseRead

{-
   parseBlock is used to parse a block with either a list
   of declarations or a block with no list of declarations

   I'm doing it this way to automatically create the right
   data structure for the parsed input.

   run parseBlock "{ [ka,adw,ok,adw,daw] };" => reads some declarations
   run parseBlock "{ [] };" => does not read any declarations
   run parseBlock "{};" => same as above
-}
parseBlock :: Parser Stmt
parseBlock = try parseBlockDecl <|> try parseBlockNoDecl

{-
   parse a block with a declaration, I'm using a C syntax with { [ ] }

   run parseBlockDecl "{};" => fail (no decl)
   run parseBlockDecl "{[dd]}" => gives a block with decl
   run parseBlockDecl "{[2]}" => fail (const in decl)
-}
parseBlockDecl :: Parser Stmt
parseBlockDecl =
   do spaces
      string "{" -- begin
      spaces
      char '['
      declLs <- many parseDecl
      char ']'
      spaces
      statementLs <- many parseStmt
      spaces
      string "}" --end
      return (Block declLs statementLs)

{-
   parse a block with no declaration

   run parseBlockNoDecl "{};" => gives block with empty decl
   run parseBlockNoDelc "{[]};" => fail (block!)
   run parseBlockNoDecl "{[do]};" => fail (block!)
-}
parseBlockNoDecl :: Parser Stmt
parseBlockNoDecl =
   do spaces
      string "{" --begin
      spaces
      statementLs <- many parseStmt
      spaces
      string "}" --end
      spaces
      return (Block [] statementLs)

{-
   parse the assignment operators

   := for constant assignment
   = for variable assignment

   run parseAssign "23=22" => fail (assigning to a constant)
   run parseAssign "a=2" => works (assign con)
   run parseAssign "a:=23" => works (assign var)
-}
parseAssign :: Parser Stmt
parseAssign =
   do varName <- parseVar
      spaces
      op <- string "=" <|> string ":="
      spaces
      intExpr <- parseIntExp
      if op == ":="
         then return (AssignVar varName intExpr)
         else return (AssignCon varName intExpr)

{-
   parse the write statement

   run parseWrite "write monkey" => works
   run parseWrite "readSomething monkey" => fail (not write)
-}
parseWrite :: Parser Stmt
parseWrite =
   do string "write"
      spaces
      varName <- parseIVar
      return (Write varName)

{-
   parser part: Read Var

   run parseRead "read monkey" => works
   run parseRead "some something" => fail (not read)
-}
parseRead :: Parser Stmt
parseRead =
   do string "read"
      spaces
      varName <- parseVar
      return (Read varName)

{-
   parse while loop,

   first a conditional
   then a block

   this is also co-recursive with the parseStmt parser so
   several while loops can be nested in each other

   run parseWhile "while() {};" => fail (no conditional)
   run parseWhile "while(a>3) ;" => fail (no block)
   run parseWhile "while(a>3) {};"
      => While (GThan (IVar "a") (ICon 3)) (Block [] [])
-}
parseWhile :: Parser Stmt
parseWhile =
   do string "while"
      conditional <- parseConditional
      nextStmt <- parseBlock
      spaces
      return (While conditional nextStmt)

{-
   parse a conditional

   first (
   then a boolean expression
   then )

   run parseConditional "a>3" => fail (no parenthesis, I
      want my syntax to be that way)
   run parseConditional "(a>4)" => GThan (IVar "a") (ICon 4)
-}
parseConditional :: Parser BoolExp
parseConditional =
   do spaces
      char '('
      spaces
      cond <- parseBool
      spaces
      char ')'
      spaces
      return cond

{-
   parse an if statement

   if followed by a conditional, a block ...
      followed by an else and a block

   run parseIfThenElse "if (a>b) {i:=2;} else {read w;};"
      => gives a syntax tree
   run parseIfThenElse "if () {i:=2;} else {read w;};"
      => fail (no  conditional)
   run parseIfThenElse "if (a>b) {i:=2;} else ;"
      => fail (no block in else)
   run parseIfThenElse "if (a<b) else {read w;};"
      => fail (no block in if)
-}
parseIfThenElse :: Parser Stmt
parseIfThenElse =
   do spaces
      string "if"
      conditional <- parseConditional
      true <- parseBlock
      string "else"
      false <- parseBlock
      spaces
      return (IfThenElse conditional true false)


{-
   Parse the declarations. either a declarations has
   more after it, or not.

   run parseDecl "ww" => ww
   run parseDecl "ww," => ww
-}
parseDecl :: Parser Decl
parseDecl = try parseDeclComma <|> try parseDeclNoComma

parseDeclComma :: Parser Decl
parseDeclComma =
   do spaces
      varName <- parseVar
      spaces
      char ','
      return varName

parseDeclNoComma :: Parser Decl
parseDeclNoComma =
   do spaces
      varName <- parseVar
      spaces
      return varName

{-
   Interpreter:

   My interpreter passes the state as a tuple of:
   (syntaxTree, environment)

   IO Monad
   Environment = Set of vars in scope along with current values
   (var,val)
   [(Var,Int])
-}

type Env = [[(Var,Int)]]

{-
   Use read from the Maybe monad and return
   iint or error

   lookUp "w" [[("w",3)]] => 3
   lookUp "w" [[("x",2)],[("y",4)],[("w",4)]] => 4
   lookUp "t" [[("x",2)],[("t",6)],[("w",4)]] => 6
   lookUp "w" [[("x",3)]] => fail (not in env)
-}
lookUp :: Var -> Env -> Int
lookUp var env | isJust mayb = fromJust mayb
               | otherwise   = error "Var not in environment"
                 where mayb = lookUp' var env

{-
   look something up in a given environment
   using lookUpInd for individual lookup
-}
lookUp' :: Var -> Env -> Maybe Int
lookUp' var []     = Nothing
lookUp' var (x:xs) | isJust mayb = mayb
                   | otherwise   = lookUp' var xs
                     where mayb  = lookUpInd var x

{-
   lookup for individual list
-}
lookUpInd :: Var -> [(Var,Int)] -> Maybe Int
lookUpInd var []     = Nothing
lookUpInd var (x:xs) | var == v    = Just i
                     | otherwise   = lookUpInd var xs
                       where (v,i) = (fst x,snd x)

{- update all variables in all scopes with the same name
   using lookUp' to see if the var is already in the environment
    -> in that case, just update var
    -> if not in the environment, add it at the last place to
         the last part of the environment

   update "w" 4 [[("w",3),("c",4)],[("d",9)]]
      => [[("w",4),("c",4)],[("d",9)]]
   update "w" 5 [[("x",3),("c",4)],[("d",9)]]
      => [[("x",3),("c",4)],[("d",9),("w",5)]]
-}
update :: Var -> Int -> Env -> Env
update var val env
   | isJust (lookUp' var env) =
      [ [ update' (v,i) (var,val) | (v,i) <- envList ]
      | envList <- env ]
   | otherwise = init env ++ [last env ++ [(var,val)]]

{-
   update a pair of (Var,Int) if the new var == the old var
-}
update' :: (Var,Int) -> (Var,Int) -> (Var,Int)
update' (ov,oi) (nv,ni) | nv == ov  = (nv,ni)
                        | otherwise = (ov,oi)
{-
    state handling below!!!! –—————————
-}
writeStuff :: StateT (Stmt,Env) IO ()
writeStuff =
   do sputStrLn "Beginning interpretation of the calgol program"
      stateAndEnv <- get
      decideOnStmt $ fst stateAndEnv
      return ()

{-
   Helper function, recursively update many variables
-}
updateMany :: [(Var,Int)] -> Env -> Env
updateMany []     env = env
updateMany (x:xs) env = updateMany xs (update (fst x) (snd x) env)

{-
   This is a helper function using decideOnStmt.

   Decide what to do with a list of statements

   Each of these statement might trigger decideOnStatement
   to do different things.

   This makes a big monadic binding I think. causing a
   sequence to be lined up
-}
decideOnStmtMany :: [Stmt] -> StateT (Stmt,Env) IO ()
decideOnStmtMany []     =
   do return ()
decideOnStmtMany (x:xs) =
   do decideOnStmt x
      decideOnStmtMany xs
      return () --ta bort för den kan komma flera ggr?

{-
   This is the main work horse of the interpreter.

   Given any sort of Stmt type ,it will pattern match
   against it, and do something appropriate with it.

   The statement monad will hold the current state (including
   environment)of the program. And the function will
   typically fetch the state and modify it in a single
   function call.
-}
decideOnStmt :: Stmt -> StateT (Stmt,Env) IO ()
decideOnStmt (Block decl stmt) =
   do stateAndEnv <- get
      let decl = getDeclFromBlock (fst stateAndEnv)
      let stmt = getStmtFromBlock (fst stateAndEnv)
      -- add a new scope to the block
      let newEnv = updateMany (zip decl (repeat 0)) (snd stateAndEnv ++ [[]])
      put (fst stateAndEnv, newEnv)
      decideOnStmtMany stmt
      stateANdEnv2 <- get
      put (fst stateAndEnv, init $ snd stateANdEnv2) -- remove the blocks' scope
      return ()
decideOnStmt (AssignVar var intExp) =
   do stmtAndEnv <- get
      let intCalced = evalIntExp intExp (snd stmtAndEnv)
      put (fst stmtAndEnv, update var intCalced (snd stmtAndEnv))
      return ()
decideOnStmt (Read var) =
   do sputStrLn ("Please enter the value for Var: " ++ var)
      val <- sgetLine
      let valInt = read val::Int
      stmtAndEnv <- get
      let updEnv = update var valInt (snd stmtAndEnv)
      put (fst stmtAndEnv,updEnv)
      return ()
decideOnStmt (Write intExp) =
   do stmtAndEnv <- get
      sputStrLn ("Writing variable: " ++ show intExp ++ " -> " ++
                 show (evalIntExp intExp (snd stmtAndEnv)))
      put stmtAndEnv
      return ()
decideOnStmt (IfThenElse boolExp stmt stmt2) =
   do stmtAndEnv <- get
      let boolResult = evalBool boolExp (snd stmtAndEnv)
      if boolResult then
         do put (stmt, snd stmtAndEnv)
            decideOnStmt stmt
            newState <- get --is this really needed, is get a stack or smthn?
            put (fst stmtAndEnv, snd newState)
            return()
      else
         do put (stmt2, snd stmtAndEnv)
            decideOnStmt stmt2
            newState <- get
            put (fst stmtAndEnv, snd newState)
            return()
decideOnStmt (While boolExp stmt) =
   do stmtAndEnv <- get
      put (stmt,snd stmtAndEnv)
      runWhileLoop boolExp
      return ()

{-
   while loop
   separated it because it was easier for me to think
   when it was alone. Now I can just store away the env
   once and then everything inside the block happens here
   and the conditional (bool::BoolExp) never changes but
   is evaluated every run of the loop
-}
runWhileLoop :: BoolExp -> StateT (Stmt,Env) IO ()
runWhileLoop bool =
   do stmtAndEnv <- get
      let boolResult = evalBool bool (snd stmtAndEnv)
      if boolResult then
         do decideOnStmt (fst stmtAndEnv)
            runWhileLoop bool
            newState <- get
            put (fst stmtAndEnv, snd newState)
      else
         return ()

{-
   Simple helper method getting the declaration part
   of a block data type
-}
getDeclFromBlock :: Stmt -> [Decl]
getDeclFromBlock (Block decl stmt) = decl

{-
   Simple helper method getting the statement part
   if a block data type
-}
getStmtFromBlock :: Stmt -> [Stmt]
getStmtFromBlock (Block decl stmt) = stmt

{-
   Used to evaluate IntExp expressions recursively

   it's required to pass an environment because
   the evaluator checks variables agains t

   evalIntExp (Mul (ICon 2) (Mul (ICon 4) (ICon 3))) [[("w",3)]]
      => 24
   evalIntExp (ICon 2) => 2 (base case)
-}
evalIntExp :: IntExp -> Env -> Int
evalIntExp (IVar var)  env = lookUp var env
evalIntExp (ICon int)  env = int
evalIntExp (Add e1 e2) env = (evalIntExp e1 env) +     (evalIntExp e2 env)
evalIntExp (Sub e1 e2) env = (evalIntExp e1 env) -     (evalIntExp e2 env)
evalIntExp (Mul e1 e2) env = (evalIntExp e1 env) *     (evalIntExp e2 env)
evalIntExp (Div e1 e2) env = (evalIntExp e1 env) `div` (evalIntExp e2 env)

{-
   Simply evaluate both sides and use haskells built in operators
-}
evalBool :: BoolExp -> Env -> Bool
evalBool (LThan  e1 e2) env = (evalIntExp e1 env) <  (evalIntExp e2 env)
evalBool (Equals e1 e2) env = (evalIntExp e1 env) == (evalIntExp e2 env)
evalBool (GThan  e1 e2) env = (evalIntExp e1 env) >  (evalIntExp e2 env)

{-
   do IO inside the StateT monad
   (lift the io to the inner monad in StateT)
-}
sputStrLn :: String -> StateT a IO ()
sputStrLn xs = liftIO (putStrLn xs)

{-
   do IO inside the StateT monad
   (lift the io to the inner monad in StateT)
-}
sgetLine :: MonadIO m => m String
sgetLine = liftIO getLine

{-
   debug function: print a state
-}
printState :: Show a => StateT a IO ()
printState =
   do s <- get
      sputStrLn ("Current state is " ++ show s)

{-
   The main usage file from outside of this module.
   Given a file, interpret the program!
-}
program :: FilePath -> IO()
program fp =
   do parsed <- parseFromFile parseStmt fp --parse from file
      case parsed of
         Left err ->
            do putStrLn "Error parsing the program at:"
               print err
         Right x ->
            do putStrLn "Parsing completed!"
               (a,s) <- runStateT writeStuff (x,[])
               return ()

{-
   Well a copy from the one above but I include it
   just to be able to show some simple programs...

   please be sure to remove any weird tabs caused by the multiple lines
   when running the unit test below. at least I get those when
   copying from vim

   programFromText "{
                     [a,b,c,d,e,z]
                     read d;
                     b := 23;
                     a := 34;
                     c:= a+b;
                     write c;
                     if(d>100) {
                       [qw,ewr]
                       c:=d;
                       qw := 1;
                       ewr := 2*2*4*1*4*2*1+1;
                       write qw;
                       write ewr;
                     }else{
                       e:=c;
                       qw := 3;
                       ewr := 4;
                       write qw;
                       write ewr;
                     };

                     while(a < 36) {
                       a:=a+1;
                       write a;
                       while(z < 100) {
                         z := z+1;
                       };
                     };
                    };"

   programFromText "{
                     [a,b,c,n,i,apa,bepa]
                     read i;
                     read a;
                     n := 1;
                     while (i>0) {
                       n := 2*n;
                       i := i-1;
                       if(apa==bepa) {
                         read apa;
                         while(a==b) {
                           write apa;
                         };
                       } else {
                         write bepa;
                       };
                     };
                     write n;
                     i := 2*3;
                     while(i==3) {
                       [w]
                       if(a==2){
                         i:=n;
                       }else{
                         i:=2;
                       };
                     };
                   };"
-}
programFromText :: String -> IO()
programFromText st =
   do let parsed = parse parseStmt "" st
      case parsed of
         Left err ->
            do putStrLn "Error parsing the program at:"
               print err
         Right x ->
            do putStrLn "Parsing completed!"
               (a,s) <- runStateT writeStuff (x,[])
               return ()

{-
   for testing a parser ———————————————
   stolen from course example files ^_^
-}
run :: Show a => Parser a -> String -> IO ()
run p input
   = case parse p "" input of
      Left err ->
         do putStr "parse error at "
            print err
      Right x -> print x
