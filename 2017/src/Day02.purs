module Day02 where

import Data.String (split, Pattern(..))
import Data.Array (catMaybes, filter, null, concat)
import Data.Int (fromString)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl, sum, minimum, maximum)
import Data.Traversable (sequence)
import Control.MonadZero (guard)
import Base

parse :: String -> Array (Array Int)
parse = split (Pattern "\n") >>> map (split (Pattern " ") >>> map fromString >>> catMaybes) >>> filter (not null) 

solve1 :: String -> Int
solve1 =
  parse >>> map (\xs -> (-) <$> maximum xs <*> minimum xs) >>> sequence >>> fromMaybe [] >>> sum
  
solve2 :: String -> Int
solve2 =
  parse >>> map division >>> concat >>> sum
  where
    division xs = do 
      x <- xs
      y <- xs
      guard $ x /= y && x > y && x `mod` y == 0
      pure $ x / y

main = runTest do
  test "day 2 part 1" do
    equal 18 $ solve1 "5 1 9 5\n7 5 3\n2 4 6 8"
  test "day 2 part 1" do
    equal 9 $ solve2 "5 9 2 8\n9 4 7 3\n3 8 6 5\n"
  test "input" do
    input <- readFile "./inputs/Day02.txt"
    log $ show $ solve1 input
    log $ show $ solve2 input


