{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Day17 where

import Data.List (foldl')
import qualified Data.Set as Set

type Point = (Int, Int, Int, Int)
type Space = [Point]

instance Num Point where
    (a,b,c,d) + (p,q,r,s) = (a+p,b+q,c+r,d+s)
    negate (a,b,c,d) = (-a,-b,-c,-d)


mkSpace :: Point -> Point -> Space
mkSpace (sx, sy, sz, sw) (ex, ey, ez, ew) = (,,,) <$> [sx..ex] <*> [sy..ey] <*> [sz..ez] <*> [sw..ew]

neighbors :: Space -> Point -> [Point]
neighbors deltas point = [point + d | d <- deltas, d /= (0, 0, 0, 0)]


data HyperCube = HyperCube { start :: Point
                           , end   :: Point
                           , cells :: Set.Set Point
                           }

mkCells ks vs = Set.fromList [point | (point, isAlive) <- zip ks vs, isAlive]

mkHyperCube :: [[Char]] -> HyperCube
mkHyperCube field =
    let l = length field
        w = length $ head field
        coords = mkSpace (1, 1, 1, 1) (l, w, 1, 1)
        values = map (== '#') (concat field)
    in
        HyperCube (1, 1, 1, 1) (l, w, 1, 1) (mkCells coords values)

-- a step in the conway hypercube
step delta (HyperCube start end cells) =
    let coords = mkSpace (start - delta) (end + delta)
        values = map willBeAlive coords
        cells' = mkCells coords values
        start' = foldl' (elemWise min) maxBound cells'
        end'   = foldl' (elemWise max) minBound cells'
    in
        HyperCube start' end' cells'
    where
        unitCube = mkSpace (-delta) delta
        isAlive = (`Set.member` cells)
        neighborsAlive = sum . map (fromEnum . isAlive) . neighbors unitCube
        willBeAlive point
            | isAlive point = k == 2 || k == 3  -- if cell is alive, it remains alive if exactly 2 or 3 of its neighbors are also alive.
            | otherwise     = k == 3            -- if cell is not alive, it becomes alive if exactly 3 of its neighbors are alive.
            where k = neighborsAlive point
        elemWise f (a, b, c, d) (p, q, r, s) = (f a p, f b q, f c r, f d s)


step3d = step (1, 1, 1, 0)
step4d = step (1, 1, 1, 1)

aliveCount :: HyperCube -> Int
aliveCount = length . cells

cube = mkHyperCube . lines <$> readFile "src/input/Day17.txt"

------------------------------------------------------
partOne = print . aliveCount . (!! 6) . iterate step3d =<< cube
-- Answer: 289

partTwo = print . aliveCount . (!! 6) . iterate step4d =<< cube
-- Answer: 2084

------------------------------------------------------
-- chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

{- TODOs:
    1. set of points -> list of neighbor points -> neighbor counts -> next set of points
    2. (u8, u8, u8, u8) -> u32
-}
------------------------------------------------------
