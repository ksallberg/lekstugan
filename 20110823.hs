import Test.QuickCheck
import Data.List

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Show, Enum)

-- problem 1 a:

instance Arbitrary Weekday where
    arbitrary = elements [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

prop_listSorted :: [Weekday] -> Bool
prop_listSorted inp = (sort inp) == (mysort inp)

prop_lengthKept :: [Weekday] -> Bool
prop_lengthKept inp = length (mysort inp) == length inp

-- type checking shit

mysort :: [Weekday] -> [Weekday]
mysort = undefined

-- problem 2 a:

-- a type DateSet

-- a function isIn :: Date -> DateSet -> Bool

-- a function upperBound :: DateSet -> Date

-- toList :: DateSet -> [Date]

-- start from
-- epochD = ReadD "1970-01-01"

-- assume functions
--     readD :: String -> Date
--     showD :: Date -> String

-- accessor function weekday, monthday, yearday :: Date -> Int

-- assume nextD :: Date -> Date


