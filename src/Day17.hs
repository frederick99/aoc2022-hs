{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Day17 where

import qualified Data.Set as Set
import Data.Bool (bool)

type Point = (Int, Int, Int, Int)
type Space = [Point]

instance Num Point where
    (a,b,c,d) + (p,q,r,s) = (a+p,b+q,c+r,d+s)
    negate (a,b,c,d) = (-a,-b,-c,-d)

isOrigin :: Point -> Bool
isOrigin (x, y, z, w) = x == 0 && y == 0 && z == 0 && w == 0

mkSpace :: Point -> Point -> Space
mkSpace (sx, sy, sz, sw) (ex, ey, ez, ew) = (,,,) <$> [sx..ex] <*> [sy..ey] <*> [sz..ez] <*> [sw..ew]

neighbors :: Space -> Point -> [Point]
neighbors deltas (x, y, z, w) = [(x + dx, y + dy, z + dz, w + dw) | p@(dx, dy, dz, dw) <- deltas, not (isOrigin p)]


data HyperCube = HyperCube { start :: Point
                           , end :: Point
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
    let start' = start - delta
        end' = end + delta
        coords = mkSpace start' end'
        values = map willBeAlive coords
    in
        HyperCube start' end' (mkCells coords values)
    where
        unitCube = mkSpace (-delta) delta
        isAlive = (`Set.member` cells)
        neighborsAlive = sum . map (fromEnum . isAlive) . neighbors unitCube
        willBeAlive point
            | isAlive point = k == 2 || k == 3  -- if cell is alive, it remains alive if exactly 2 or 3 of its neighbors are also alive.
            | otherwise     = k == 3            -- if cell is not alive, it becomes alive if exactly 3 of its neighbors are alive.
            where k = neighborsAlive point


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

main = partOne
------------------------------------------------------
-- showCube (HyperCube s e cells) = toString (bool '.' '#') s e cells

-- toString f (sx, sy, sz) (ex, ey, ez) cells =
--     let l = ex - sx + 1; w = ey - sy + 1; h = ez - sz + 1
--         str = f <$> Map.elems cells
--         format = unlines . map unlines . chunks l . chunks w
--     in show (l,w,h) ++ '\n' : format str
--     where
--         chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)
------------------------------------------------------
