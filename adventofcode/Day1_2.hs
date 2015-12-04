module Day1_2 (main) where

main :: IO ()
main = do
    input <- getLine
    putStrLn (show $ step input 1)

step :: String -> Int -> Int
step str floorsToTest =
    let thisRound = calcFloor (take floorsToTest str)
    in case thisRound < 0 of
          True  -> floorsToTest
          False -> step str (floorsToTest + 1)

calcFloor :: String -> Int
calcFloor [] = 0
calcFloor ('(':xs) = 1 + (calcFloor xs)
calcFloor (')':xs) = (calcFloor xs) - 1
calcFloor _ = error "No friggin floor"
