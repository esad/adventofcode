module Day7 where

import Base
import Data.String.Regex
import Data.Foldable (and)
import Data.List ((:), List(..), fromFoldable)
import Data.String (toCharArray)
import Data.Traversable (sequence)
import Data.Foldable (any)
import Data.Array (length, filter)

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
    bracketsAbba =
      case match re input |> map sequence |> join |> map (any abba)  of
        Nothing -> false
        Just r -> r
    re = regex "\\[(.*?)\\]" (parseFlags "g") |> forceRight
    
main :: Main Unit
main = do
  lines <- inputLines "./src/Day7.in"
  log $ show $ (length <<< filter supportsTLS) lines
