{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day7 where

import Data.List ( foldl' )
import qualified Data.Set as Set
import qualified Data.Map as Map

type Color = String
type Bag  = (,)     Color [(Integer, Color)]
type Bags = Map.Map Color [(Integer, Color)]

mkBag :: String -> Bag
mkBag (words -> adj:col:_:_:rest) = (adj ++' ': col, bags rest) where
    bags ("no":_) = []
    bags (n:adj:col:_:rest) = (read n, adj ++' ': col) : bags rest
    bags _ = []

bags = Map.fromList . map mkBag . lines <$> readFile "src/input/Day7.txt"

------------------------------------------------------
uniqueColors :: Bags -> Color -> Set.Set Color
uniqueColors bags color =
    color `Set.insert` maybe Set.empty
                            (unionMap $ uniqueColors bags . snd)
                            (Map.lookup color bags)

unionMap f = Set.unions . map f

partOne = bags >>= \bags -> print
    . pred . length     -- exclude the shiny gold bag
    . filter (Set.member "shiny gold" . uniqueColors bags)
    $ Map.keys bags
-- Answer: 169

countBags :: Bags -> Color -> Integer 
countBags bags color = maybe 0 (foldl' go 0) (Map.lookup color bags) where
    go acc (n, color) = n * (countBags bags color + 1) + acc

partTwo = bags >>= print . flip countBags "shiny gold"
-- Answer: 82372