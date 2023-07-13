module Day6 where

import Data.List ( foldl', groupBy )
import Data.Bits ( Bits((.&.), (.|.), shiftL, popCount) )
import Data.Monoid ( Sum(Sum, getSum) )

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = filter (notElem c) . groupBy (\a b -> (a == c) == (b == c))

sumMap f = getSum . foldMap (Sum . f)

encode :: String -> Int
encode = foldl' (\a r -> a .|. shiftL 1 (fromEnum r - fromEnum 'a')) 0

groups = splitOn "" . lines <$> readFile "src/input/Day6.txt"

--------------------------------------------------------
unique = popCount . foldl' (.|.) 0 . map encode
partOne = groups >>= print . sumMap unique
-- Answer: 6680

unanimous = popCount . foldl' (.&.) maxBound . map encode   -- assume maxBound returns a number with all 1's
partTwo = groups >>= print . sumMap unanimous
-- Answer: 3117
