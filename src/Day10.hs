module Day10 where

import Data.List ( foldl', sort )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map

joltages = map read . lines <$> readFile "src/input/Day10.txt"

------------------------------------------------------
-- | wall - 0; device - 3 higher than the highest joltage
connectToOutlet xs = sort $ 0 : (maximum xs + 3) : xs

adapterJoltageDiffs = differences . connectToOutlet
  where differences = zipWith subtract <*> tail

count x = length . filter (== x)

partOne = print
  . liftA2 (*) (count 1) (count 3)
  . adapterJoltageDiffs
  =<< joltages
-- Answer: 1914

lookUpOrZero map key = fromMaybe 0 (Map.lookup key map)

combinations = liftA2 lookUpOrZero getCountsMap maximum where
  initCounts = Map.fromList [(0, 1)]
  getCountsMap = foldl' reduce initCounts . sort
  reduce counts joltage =
    let lastThreeCounts = lookUpOrZero counts <$> [joltage - 3 .. joltage - 1]
    in Map.insert joltage (sum lastThreeCounts) counts

partTwo = print . combinations =<< joltages
-- Answer: 9256148959232
