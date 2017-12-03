module Day03 where

import Data.Foldable (minimum)
import Data.Ord (abs)

import Base

solve1 :: Int -> Int
solve1 n =
  let
    -- find the distance to the nearest pole
    d = forceJust <<< minimum $ poles <#> (\p -> abs $ n - p) 
  in
    d + k -- this distance + index of the ring we are on should be the manhattan distance to 1
  where
    sq n = n * n
    len k = k*2+1 -- length of the side of the k-th ring
    ar k = sq (len k) -- area of the k-th ring
    k = go 0 n -- index of the ring n is on
    go k n = if n <= ar k then k else go (k+1) n
    east = if k == 0 then 1 else ar (k-1) + k -- middle-right number on this ring
    d = len k - 1
    poles = [0,1,2,3] <#> (\p -> east + p * d) -- east-north-west-south numbers
    
main = runTest do
  suite "Day3" do
    test "part 1" do
      equal 0 $ solve1 1
      equal 31 $ solve1 1024
    --test "part 2" do
      --equal 9 $ solve2 "5 9 2 8\n9 4 7 3\n3 8 6 5\n"
    test "inputs" do
      log $ show $ solve1 277678

