module Day04 where

import Data.String (split, Pattern(..), null)
import Data.Set (member, insert, empty)
import Data.List (List(..), (:), fromFoldable)
import Data.Foldable (foldl)
import Data.Array (filter)
import Base


valid :: String -> Boolean
valid =
  split (Pattern " ") >>> fromFoldable >>> go empty
  where
    go seen Nil = true
    go seen (x:xs) = if x `member` seen then false else go (insert x seen) xs
  
solve1 :: String -> Int
solve1 =
  split (Pattern "\n") >>> filter (not null) >>> foldl (\acc x -> if valid x then acc+1 else acc) 0

main = runTest do
  suite "Day4" do
    test "part 1" do
      equal 1 $ solve1 "aa bb cc dd ee"
      equal 0 $ solve1 "aa bb cc dd aa\naa bb cc dd aa"
      equal 1 $ solve1 "aa bb cc dd aaa"
    test "input" do
      input <- readFile "./inputs/Day04.txt"
      log $ show $ solve1 input
