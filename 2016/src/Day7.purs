module Day7 where

import Base
import Data.String.Regex
import Data.Array (length, filter)
import Data.Foldable (and)
import Data.Foldable (any)
import Data.List ((:), List(..), fromFoldable)
import Data.Maybe (maybe)
import Data.String (toCharArray)
import Data.Traversable (sequence)

abba :: String -> Boolean
abba =
  toCharArray >>> fromFoldable >>> aux
  where
    aux (x1 : x2 : x3 : x4 : _) | x1 == x4 && x2 == x3 && x1 /= x2 = true
    aux Nil = false
    aux (_:xs) = aux xs

supportsTLS :: String -> Boolean
supportsTLS input =
  not bracketsAbba && abba input
  where
    bracketsAbba = match re input |> map sequence |> join |> map (any abba) |> fromMaybe false
    re = regex "\\[(.*?)\\]" (parseFlags "g") |> forceRight

main :: Main Unit
main = do
  lines <- inputLines "./src/Day7.in"
  log $ show $ (length <<< filter supportsTLS) lines
