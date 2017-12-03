module Day03 where

import Data.Foldable (minimum)
import Data.Ord (abs, between)
import Data.Tuple (Tuple(..))
import Data.Map (singleton, lookup, insert)
import Data.Foldable (sum)
import Data.Array (catMaybes)
import Base

sq :: Int -> Int
sq n = n * n

-- length of the side of the k-th ring
len :: Int -> Int
len k = k*2+1

-- area of the k-th ring
ar :: Int -> Int
ar = sq <<< len

-- which ring is the number on
ring :: Int -> Int
ring n = go 0 n where go k n = if n <= ar k then k else go (k+1) n

solve1 :: Int -> Int
solve1 n =
  let
    -- find the distance to the nearest pole
    d = forceJust <<< minimum $ poles <#> (\p -> abs $ n - p) 
  in
    d + k -- this distance + index of the ring we are on should be the manhattan distance to 1
  where
    k = ring n
    east = if k == 0 then 1 else ar (k-1) + k -- middle-right number on this ring
    d = len k - 1
    poles = [0,1,2,3] <#> (\p -> east + p * d) -- east-north-west-south numbers

-- Part 2

infixl 4 Tuple as &
type Pos = Tuple Int Int

coords :: Int -> Pos
coords n =
  if between br tr n then
    k & ( k - (tr - n))
  else if between tr tl n then
    (k - (n - tr)) & k
  else if between tl bl n then
    -k & (k - (n - tl))
  else
    (k - (br' - n)) & -k --
  where
    k = ring n
    d = len k - 1
    br = if k == 0 then 1 else ar (k-1) + k - d/2
    tr = br + d
    tl = br + d * 2
    bl = br + d * 3
    br' = br + d * 4

neighbours :: Pos -> Array Pos
neighbours (x & y) = 
  [
    x - 1 & y + 1, x & y + 1, x + 1 & y + 1,
    x - 1 & y    ,            x + 1 & y    ,
    x - 1 & y - 1, x & y - 1, x + 1 & y - 1
  ]

solve2 :: Int -> Int
solve2 n =
  go 2 n $ singleton (0 & 0) 1
  where
    go i n m =
      let 
        c = coords i
        s = sum <<< catMaybes $ flip lookup m <$> neighbours c
      in
        if s > n then s else go (i+1) n (insert c s m)

main = runTest do
  suite "Day3" do
    test "part 1" do
      equal 0 $ solve1 1
      equal 31 $ solve1 1024
    test "part 2" do
      equal 4 $ solve2 3
      equal 122 $ solve2 100
    test "inputs" do
      log $ show $ solve1 277678
      log $ show $ solve2 277678

