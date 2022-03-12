{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day9 where

import Data.List ( unfoldr )
import Control.Applicative ( Applicative(liftA2) )

takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _[] = Nothing
takeExact n (x:xs) = (x:) <$> takeExact (n-1) xs

hasTwoSum :: (Eq a, Num a) => a -> [a] -> Bool
hasTwoSum target (x : xs) = elem (target-x) xs || hasTwoSum target xs
hasTwoSum _ _ = False

windows :: Int -> [a] -> [[a]]
windows = unfoldr . go where
    go n xs = takeExact n xs >>= \t -> Just (t, tail xs)

find :: (a -> Bool) -> [a] -> a
find pred = head . filter pred

xmasData = map read . lines <$> readFile "src/input/Day9.txt"
preamble = 25

-- | Returns all contiguous subsequences of size >= 2
-- | FIXME: this returns an infinite list which is incorrect
subsequences = concat . liftA2 windows [2 ..] . pure

targetSumSubsequence n = find (\xs -> sum xs == n) . subsequences

sumMinMax xs = minimum xs + maximum xs

------------------------------------------------------
firstInvalidNumber preamble
    = head
    . find (not . isValid)
    . map reverse
    . windows (preamble + 1)
    where isValid (x : xs) = hasTwoSum x xs

partOne = print . firstInvalidNumber preamble =<< xmasData
-- Answer: 375054920

partTwo = print . sumMinMax . (targetSumSubsequence =<< firstInvalidNumber preamble) =<< xmasData
-- Answer: 54142584