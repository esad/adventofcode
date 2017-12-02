module Day01 where

import Base
import Data.String (toCharArray, singleton)
import Data.Int (fromString)
import Data.Array as A
import Data.Array (mapMaybe, snoc, length, (!!), uncons, take)
import Data.List (fromFoldable)

parse :: String -> Array Int
parse = toCharArray >>> mapMaybe (singleton >>> fromString)

sum :: Int -> List Int -> Int
sum acc (x:y:ys) = (if x == y then x else 0) + sum acc (y:ys)
sum acc (x:Nil) = acc
sum acc Nil = 0

solve1 :: String -> Int
solve1 s =
  let a = parse s in 
  case A.head a of
    Nothing -> 0
    Just x -> sum 0 $ fromFoldable $ a `snoc` x

sum2 :: Int -> Int -> Array Int -> Int
sum2 o acc arr | Just {head, tail} <- uncons arr =
  case arr !! o of
    Nothing -> acc
    Just y -> (if head == y then head else 0) + sum2 o acc tail
sum2 _ acc _ | otherwise = acc

solve2 :: String -> Int
solve2 s =
  let
    a = parse s
    o = length a / 2
  in sum2 o 0 $ a <> take o a 

main = runTest do
  test "day 1 part 1" do
    equal 3 $ solve1 "1122"
    equal 9 $ solve1 "91212129"
  test "day 1 part 2" do
    equal 6 $ solve2 "1212"
    equal 4 $ solve2 "123425"
    equal 4 $ solve2 "12131415"
    equal 12 $ solve2 "123123"
  test "input" do
    input <- readFile "./inputs/Day01.txt"
    log $ show $ solve1 input
    log $ show $ solve2 input


