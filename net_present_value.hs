{-
The right tool for the job. Sometimes you find really nice applications of a
language you already know - that you didn't think of when learning it. Like
when I had to calculate "net present value" for a couple of investments. The
code is pretty much what the formula looks like expressed mathematically.
-}

--Calculate npv
npv :: Float -> Float -> Float -> Float -> Float -> Integer -> Float
npv initial rf b cashflow salvage years
   = (sumNpv (capm rf b) cashflow years)
   + (salvage / (capm rf b)^years) - initial

-- Calculate the npv sum recursively
sumNpv :: Float -> Float -> Integer -> Float
sumNpv cap cashflow 0 = 0
sumNpv cap cashflow year
   = (cashflow / (cap^year))
   + (sumNpv cap cashflow (year-1))

-- Calculate capm given risk free interest and beta
capm :: Float -> Float -> Float
capm rf b = ((rf + b * ( (erM rf b) - rf )) / 100)+1

erM :: Float -> Float -> Float
erM rf b = rf + rf * b
