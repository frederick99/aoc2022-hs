module Day3 where

grid = lines <$> readFile "src/input/Day3.txt"

numTrees = countIf isTree

-- slope x y grid = zipWith ((!!) . cycle) ((grid !!) <$> [0, y .. length grid - 1]) [0, x ..]    # WIP

slope dx dy grid = zipWith (!!) (rows grid) (iterate nextCol 0)
  where
    rows = map head . takeWhile (not.null) . iterate (drop dy)
    nextCol c = (c + dx) `mod` width
    width = length $ head grid

isTree = (== '#')
countIf = (length .) . filter

partOne = grid >>= print . numTrees . slope 3 1
-- Answer: 216

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

partTwo = grid >>= print . product . map numTrees . (zipWith slope xs ys <*>) . pure
  where (xs, ys) = unzip slopes
-- Answer: 6708199680