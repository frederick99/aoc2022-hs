{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- user: nel_tu_
-- path: https://atcoder.jp/contests/abc190/tasks/abc190_c
-- tags: implementation

{- Problem Statement
We have N dishes numbered 1,2,…,N and M conditions numbered 1,2,…,M.
Condition i is satisfied when both Dish Ai and Dish Bi have (one or more) balls on them.
There are K people numbered 1,2,…,K. Person i will put a ball on Dish Ci or Dish Di
At most how many conditions will be satisfied?

Constraints:
    All values in input are integers.
    2 ≤ N ≤ 100
    1 ≤ M ≤ 100
    1 ≤ Ai < Bi ≤ N
    1 ≤ K ≤ 16
    1 ≤ Ci < Di ≤ N

Input:
    N M
    A1 B1
    ⋮
    AM BM
    K
    C1 D1
    ⋮
    CK DK
-}


-- https://codeforces.com/contest/1465/problem/B

digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

nonZeroDigits = filter (/= 0) . digits

divisibleBy x m = x `mod` m == 0

isFair = all . divisibleBy <*> nonZeroDigits

nextFair = head . filter isFair . enumFrom

main = interact $ unlines . map (show . nextFair . read) . tail . lines











{-
-- user: nel_tu_
-- path: https://codeforces.com/contest/1535/problem/A
-- tags: implementation

import Data.Bool ( bool )

isFair :: [Int] -> Bool
isFair [a, b, c, d]
    | max a b < min c d = False
    | max c d < min a b = False
    | otherwise         = True

main = interact $ unlines . map solve . tail . lines
    where solve = bool "NO" "YES" . isFair . map read . words
-}


{-
-- user: nel_tu_
-- path: https://codeforces.com/contest/1535/problem/B
-- tags: implementation

{-# LANGUAGE TupleSections #-}
import Control.Monad ( replicateM_ )
import Data.List ( sortOn )

evensThenOdds = sortOn (`mod` 2)

pairs (x : xs) = map (x,) xs ++ pairs xs
pairs _ = []

maxGoodIndices :: [Int] -> Int
maxGoodIndices = length . filter isGood . pairs . evensThenOdds
    where isGood (a, b) = gcd a (2 * b) > 1

main = do n <- read <$> getLine
          replicateM_ n (getLine >> getLine >>= print . solve)
    where solve = maxGoodIndices . map read . words
--}


-- user: nel_tu_
-- path: https://codeforces.com/contest/1535/problem/C
-- tags: implementation




{- Samples
4 4
1 2
1 3
2 4
3 4
3
1 2
1 3
2 3

2


4 4
1 2
1 3
2 4
3 4
4
3 4
1 2
2 4
2 4

4


6 12
2 3
4 6
1 2
4 5
2 6
1 5
4 5
1 3
1 2
2 6
2 3
2 5
5
3 5
1 4
2 6
4 6
5 6

9
-}
