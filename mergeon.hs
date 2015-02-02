import Data.List

mergeOn :: Ord a => (t -> a) -> [t] -> [t] -> [t]
mergeOn f [] ys = ys
mergeOn f xs [] = xs
mergeOn f (x:xs) (y:ys)  | f x <= f y  =  x : mergeOn f xs (y:ys)
                         | otherwise   =  y : mergeOn f (x:xs) ys

type Width = Int
width :: String -> Int
width "" = 0
width s = maximum $ map length $ lines s

exam = mergeOn width ["hej", "hajaa"] ["hoja", "hijwad"]
my   = alt ["hej", "hajaa"] ["hoja", "hijwad"]

alt x y = map snd $sort $ zip (map length (x ++ y)) (x++y)
