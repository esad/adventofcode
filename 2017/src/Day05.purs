module Day05 where

import Data.Array ((!!), updateAt)
import Base

solve1 :: String -> Int
solve1 = 
  parseInts >>> jump 0 0
  where
    jump steps ip program = 
      case program !! ip of
        Nothing -> steps
        Just n -> jump (steps+1) (ip+n) (updateAt ip (n+1) program # forceJust)

main = runTest do
  suite "Day5" do
    test "part 1" do
      equal 5 $ solve1 "0\n3\n0\n1\n-3\n"
    -- test "part 2" do
    --   equal 0 $ solve2 "oiii ioii iioi iiio\nabcde xyz ecdab\n"
    --   equal 3 $ solve2 "abcde fghij\na ab abc abd abf abj\niiii oiii ooii oooi oooo\n"
    test "input" do
      input <- readFile "./inputs/Day05.txt"
      log $ show $ solve1 input
      -- log $ show $ solve2 input
