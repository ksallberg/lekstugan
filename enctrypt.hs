import Data.Char (ord, toUpper)
import Data.Bits
import Data.List
import Numeric

type PlainText = String

keyFormula :: Int -> Int
keyFormula char = (69 * char + 113) `mod` 256

encrypt :: PlainText -> Int -> [Int]
encrypt txt start = map (uncurry xor) $ zip plainTextAscii key
    where plainTextAscii = map ord txt
          key            = take (length txt) $ iterate keyFormula start

toHex :: [Int] -> String
toHex ans = let joined = intercalate " " $ map ((flip showHex "")) ans
            in map toUpper joined

q7 :: String
q7 = toHex $ encrypt "Safe" 43
