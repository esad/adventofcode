module Day2P2 where

import Prelude
import Data.Tuple
import Data.Maybe
import Data.Traversable
import Data.String
import Data.Array
import Data.Bifunctor (bimap)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Sync (readTextFile)

type Position = Tuple Int Int

pinPad :: Array (Array Char)
pinPad = """
..1..
.234.
56789
.ABC.
..D..
""" 
  # trim # split (Pattern "\n") # map toCharArray

pinPadLookup :: Position -> Maybe Char
pinPadLookup (Tuple x y) = do
  row <- pinPad !! y
  char <- row !! x
  if char == '.' then Nothing else Just char

move :: Char -> Position -> Position
move c pos =
  pos'
  # pinPadLookup
  -- If lookup returns nothing, this means we can't move there so we return previous position.
  -- If it returns a char, we map it to `const pos'`, returning new position.
  # maybe pos (const pos')
  where
    pos' =
      c
      # flip lookup [ Tuple 'U' (Tuple 0 (-1)), Tuple 'D' (Tuple 0 1), Tuple 'L' (Tuple (-1) 0), Tuple 'R' (Tuple 1 0)]
      # fromMaybe (Tuple 0 0)
      # add pos

moveStr :: Position -> String -> Position
moveStr pos str =
  foldl (flip move) pos (toCharArray str)

solve :: String -> Array (Maybe Char)
solve input = 
  input
  # split (Pattern "\n")
  # scanl moveStr (Tuple 0 2) -- "5"
  # map pinPadLookup

main :: forall eff. Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = do
  input <- readTextFile UTF8 "./src/Day2.in"
  log $ show $ solve input