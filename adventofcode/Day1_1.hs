module Day1_1 (main) where

main :: IO ()
main = do
    input <- getLine
    putStrLn (show $ calcFloor input)

calcFloor :: String -> Int
calcFloor [] = 0
calcFloor ('(':xs) = 1 + (calcFloor xs)
calcFloor (')':xs) = (calcFloor xs) - 1
calcFloor _ = error "No friggin floor"
