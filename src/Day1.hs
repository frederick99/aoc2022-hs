module Day1 where

expenses = map read . lines <$> readFile "src/input/Day1.txt"

pairs (x : xs) = map ((x :) . return) xs ++ pairs xs
pairs _ = []

pairSum target = head . filter ((target ==) . sum) . pairs

partOne = expenses >>= print . product . pairSum 2020
-- Answer: 651651

triplets (x : xs) = map (x :) (pairs xs) ++ triplets xs
triplets _ = []

tripletSum target = head . filter ((target ==) . sum) . triplets

partTwo = expenses >>= print . product . tripletSum 2020
-- Answer: 214486272