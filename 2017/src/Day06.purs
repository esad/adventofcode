module Day06 where

import Data.Array (head, mapWithIndex, length, snoc, elemIndex)
import Data.Tuple
import Data.FoldableWithIndex (foldlWithIndex)
import Base

maximumWithIndex :: Array Int -> Maybe (Tuple Int Int)
maximumWithIndex xs =
  head xs <#> \x -> foldlWithIndex compare (Tuple x 0) xs
  where
    compare i m@(Tuple mel mi) el = if el > mel then Tuple el i else m

redist :: Array Int -> Array Int
redist xs =
  case maximumWithIndex xs of
    Nothing -> []
    Just (Tuple n s) ->
      let
        l = length xs
        unitsAt i =
          reps
          + (if between (s+1) (s+rest) i then 1 else 0)
          + (if i < start then 1 else 0)
        rest = min (l - s - 1) n
        reps = (n - rest) / l
        start = (n - rest) `mod` l
      in mapWithIndex (\i x -> (if i == s then 0 else x) + unitsAt i) xs 

steps :: Array Int -> Tuple Int Int
steps xs =
  go 1 [xs] xs
  where
    go i seen xs =
      let xs' = redist xs in
      case elemIndex xs' seen of
        Nothing -> go (i+1) (seen `snoc` xs') xs'
        Just idx -> Tuple (length seen - idx) i

solve1 :: String -> Int
solve1 = parseInts >>> steps >>> snd

solve2 :: String -> Int
solve2 = parseInts >>> steps >>> fst

main = runTest do
  suite "Day5" do
    test "part 1" do
      equal 5 $ solve1 "0\n2\n7\n0"
      equal 4 $ solve2 "0\n2\n7\n0"
    test "input" do
      input <- readFile "./inputs/Day06.txt"
      log $ show $ solve1 input
      log $ show $ solve2 input
