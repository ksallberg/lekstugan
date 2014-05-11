{-# LANGUAGE MultiWayIf, LambdaCase #-}

main :: IO ()
main = do let a = 233
          testLambdaCase 2
          if | a == 23  -> putStrLn "one"
             | a == 83  -> putStrLn "tva"
             | a == 133 -> putStrLn "tre"
             | a == 233 -> putStrLn "fyr"

testLambdaCase :: Int -> IO ()
testLambdaCase a = (\case
                      1 -> putStrLn "haj"
                      2 -> putStrLn "ko"
                      3 -> putStrLn "apa") a

test :: Int
test = (\x->x+1) 2
