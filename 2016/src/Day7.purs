module Day7 where

import Base
import Data.String.Regex
import Data.Array (length, filter)
import Data.Foldable (and)
import Data.Foldable (any)
import Data.List ((:), List(..), fromFoldable)
import Data.Maybe (maybe)
import Data.String (fromCharArray, toCharArray, contains, Pattern(..))
import Data.Traversable (sequence)

abba :: String -> Boolean
abba =
  toCharArray >>> fromFoldable >>> aux
  where
    aux (x1 : x2 : x3 : x4 : _) | x1 == x4 && x2 == x3 && x1 /= x2 = true
    aux Nil = false
    aux (_:xs) = aux xs

-- Searches for occurences of aba, and returns matched babs
babs :: String -> List String
babs =
  toCharArray >>> fromFoldable >>> (aux Nil)
  where
    aux l (x1 : xs@(x2 : x3 : _)) | x1 == x3 && x1 /= x2 = aux (fromCharArray [x2,x1,x2] : l) xs
    aux l Nil = l
    aux l (_:xs) = aux l xs

re :: Regex
re = regex "\\[(.*?)\\]" (parseFlags "g") |> forceRight

supportsTLS :: String -> Boolean
supportsTLS input =
  not bracketsAbba && abba input
  where
    bracketsAbba = match re input |> map sequence |> join |> map (any abba) |> fromMaybe false

supportsSSL :: String -> Boolean
supportsSSL input =
  supernets |> babs |> any \bab -> any (contains $ Pattern bab) hypernets
  where
    hypernets = match re input |> map sequence |> join |> fromMaybe []
    supernets = replace re "" input

main :: Main Unit
main = do
  lines <- inputLines "./src/Day7.in"
  log $ show $ length $ filter supportsTLS lines
  log $ show $ length $ filter supportsSSL lines
