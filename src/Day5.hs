{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day5 where

import Data.List ( foldl' )

type Seat = String
type SeatID = Int

mkSeat :: Seat -> SeatID
mkSeat = foldl' ((.go) . (+) . (*2)) 0 where
  go 'F' = 0
  go 'B' = 1
  go 'L' = 0
  go 'R' = 1

seats = map mkSeat . lines <$> readFile "src/input/Day5.txt"

partOne = seats >>= print . maximum
-- Answer: 935

missing list = expected - actual where
  expected = sumN last - sumN first + first
  actual = sum list
  first = minimum list
  last = maximum list
  sumN n = n * (n + 1) `div` 2

partTwo = seats >>= print . missing
-- Answer: 743
