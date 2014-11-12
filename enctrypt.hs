import Data.Char (ord, toUpper)
import Data.Bits
import Data.List
import Numeric

type PlainText = String

cong :: Int -> Int
cong char = (69 * char + 113) `mod` 256

answer :: PlainText -> Int -> [Int]
answer txt start = map (uncurry xor) $ zip plainTextAscii key
    where plainTextAscii = map ord txt
          key            = take (length txt) $ iterate cong start

hexAnswer :: [Int] -> String
hexAnswer ans = let toHex  = (flip showHex) ""
                    joined = intercalate " " $ map toHex ans
                in map toUpper joined

q7 :: String
q7 = hexAnswer $ answer "Safe" 43
