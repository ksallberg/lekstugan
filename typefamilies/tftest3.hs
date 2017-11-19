{-# LANGUAGE TypeFamilies #-}

data family XList a

data instance XList Char = XCons !Char !(XList Char) | XNil
data instance XList ()   = XListUnit !Int

-- grisigt, man kan inte skapa Show av familjen
--instance Show (XList a) where
--    show XNil = "hej"

format' :: XList Char -> String
format' XNil              = "nil"
format' (XCons elem rest) = [elem] ++ ", " ++ format' rest

format'' :: XList () -> String
format'' (XListUnit x) = concat ["()" | _ <- [1..x]]

{--
grisigt, man kan inte ha en och samma funktion för att göra
print på två instanser av samma datafamilj. samma problem som
i instance Show (XList a)

format :: XList a -> String
format (XListUnit _) = "hej"--"[" ++ format' x ++ "]"
format (XNil) = "nil"
--}
test       = XNil
someVector = XCons 'a' (XCons 'c' XNil)
someOther  = XListUnit 10

main :: IO ()
main = do let a = format'  test
              b = format'  someVector
              c = format'' someOther
          putStrLn "Alla:"
          putStrLn a
          putStrLn b
          putStrLn c

type family Elem c :: *
type instance Elem [e] = e

testType :: Elem [Int]
testType = 23
