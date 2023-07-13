module Day2 where

import Data.Char ( isDigit )

data Policy = Policy { letter :: Char
                     , minFreq :: Int
                     , maxFreq :: Int }
type Password = String
data Log = Log Policy Password

instance Read Policy where
  readsPrec _ input
    | (lo, '-':rest) <- span isDigit input
    , (hi, ' ':letter:rest) <- span isDigit rest
      = [(Policy letter (read lo) (read hi), rest)]
    | otherwise = []

instance Read Log where
  readsPrec _ input
    | ((policy, rest) : _) <- reads input
    , ':':' ':password <- rest
      = [(Log policy password, "")]
    | otherwise = []

------------------------------------------------------
policies = map read . lines <$> readFile "src/input/Day2.txt"

countIf pred = length . filter pred
between a b x = a <= x && x <= b

isValid1 (Log (Policy letter low high) password) = between low high freq
  where freq = countIf (== letter) password

partOne = policies >>= print . countIf isValid1
-- Answer: 536

isValid2 (Log (Policy letter a b) password) = k == 1
  where
    k = countIf id (zipWith go [1..] password)
    go pos elem = (pos == a || pos == b) && elem == letter

partTwo = policies >>= print . countIf isValid2
-- Answer: 558
