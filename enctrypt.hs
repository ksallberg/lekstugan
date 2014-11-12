import Data.Char (ord, toUpper)
import Data.Bits
import Data.List
import Numeric

cong :: Int -> Int
cong input = (69 * input + 113) `mod` 256

answer :: [Int]
answer = map (uncurry xor) $ zip (map ord "Safe") (take 4 (iterate cong 43))

hexAnswer :: String
hexAnswer = map toUpper $ intercalate " " $ map ((flip showHex) "") answer
