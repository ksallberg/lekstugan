module Financial (presentValue,npv) where

{- Present Value
   of a stream of future cashflow

   list: the list of cashflows that is returned
         every annum

   interest: the interest a bank will give you
             for keeping your money
   
   give interest as 0->1 (5% = 0.05)
-}
presentValue :: [Double] -> Double -> Double
presentValue ls interest = presentValue' ls interest 0

presentValue' :: [Double] -> Double -> Int -> Double
presentValue' []     interest year = 0
presentValue' (x:xs) interest year =
   (presentValue' xs interest (year+1))
   + x * (1 + interest)^year

{-
   Net Present Value

   Initial cost
   risk free interest (guaranteed by a bank etc)
   b = beta value (the risk compared to the average
                   investment's risk) Average risky
                   projects have a beta value of 1
   the cashflow received per annum
   salvage = what can be sold off after the
             investment/project is finished
   years = how long the project will span
-}
npv :: Double -> Double -> Double -> Double -> Double -> Double -> Double
npv initial rf b cashflow salvage years
   = (sumNpv (capm rf b) cashflow years)
     + (salvage / (capm rf b)**years) - initial

sumNpv :: Double -> Double -> Double -> Double
sumNpv cap cashflow 0 = 0
sumNpv cap cashflow year
   = (cashflow / (cap**year)) 
     + (sumNpv cap cashflow (year-1))

capm :: Double -> Double -> Double
capm rf b = ((rf + b * ( (erM rf b) - rf )) / 100)+1

erM :: Double -> Double -> Double
erM rf b = rf + rf * b
