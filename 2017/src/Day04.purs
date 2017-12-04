module Day04 where

import Data.String (split, Pattern(..), null, joinWith, fromCharArray, toCharArray)
import Data.Set (member, insert, empty)
import Data.List (List(..), (:), fromFoldable)
import Data.Foldable (foldl)
import Data.Array (filter, sort)
import Base


valid :: String -> Boolean
valid =
  split (Pattern " ") >>> fromFoldable >>> go empty
  where
    go seen Nil = true
    go seen (x:xs) = if x `member` seen then false else go (insert x seen) xs
  
solve1 :: String -> Int
solve1 =
  lines >>> foldl (\acc x -> if valid x then acc+1 else acc) 0

solve2 :: String -> Int
solve2 =
  lines >>> map sortWords >>> joinWith "\n" >>> solve1
  where
    sortWords = split (Pattern " ") >>> map (toCharArray >>> sort >>> fromCharArray) >>> joinWith " "

main = runTest do
  suite "Day4" do
    test "part 1" do
      equal 1 $ solve1 "aa bb cc dd ee"
      equal 0 $ solve1 "aa bb cc dd aa\naa bb cc dd aa"
      equal 1 $ solve1 "aa bb cc dd aaa"
    test "part 2" do
      equal 0 $ solve2 "oiii ioii iioi iiio\nabcde xyz ecdab\n"
      equal 3 $ solve2 "abcde fghij\na ab abc abd abf abj\niiii oiii ooii oooi oooo\n"
    test "input" do
      input <- readFile "./inputs/Day04.txt"
      log $ show $ solve1 input
      log $ show $ solve2 input
