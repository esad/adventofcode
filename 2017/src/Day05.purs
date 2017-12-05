module Day05 where

import Data.Array ((!!), updateAt)
import Base

run :: (Int -> Int) -> Int -> Int -> Array Int -> Int
run inc steps ip program = 
  case program !! ip of
    Nothing -> steps
    Just n -> run inc (steps+1) (ip+n) (updateAt ip (inc n) program # forceJust)

solve1 :: String -> Int
solve1 = 
  parseInts >>> run (_ + 1) 0 0

solve2 :: String -> Int
solve2 =
  parseInts >>> run (\x -> if x >= 3 then x - 1 else x + 1) 0 0

main = runTest do
  suite "Day5" do
    test "part 1" do
      equal 5 $ solve1 "0\n3\n0\n1\n-3\n"
    test "part 2" do
      equal 10 $ solve2 "0\n3\n0\n1\n-3\n"
    test "input" do
      input <- readFile "./inputs/Day05.txt"
      log $ show $ solve1 input
      log $ show $ solve2 input
