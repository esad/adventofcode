module Day2 where

import Prelude
import Data.Tuple
import Data.Maybe
import Data.Traversable
import Data.String
import Data.Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Sync (readTextFile)

moveStr :: Int -> String -> Int
moveStr pos str =
  foldl (flip move) pos (toCharArray str)

-- I know, this got pretty convoluted
move :: Char -> Int -> Int
move =
  fromMaybe id <<< (flip lookup) [ Tuple 'U' (ud (-3)), Tuple 'D' (ud 3), Tuple 'L' (lr (-1)), Tuple 'R' (lr 1)]
  where
    ud step x = if r >= 1 && r <= 9 then r else x where r = x + step
    lr step x = if (r - 1) / 3 == (x - 1) / 3 then ud step x else x where r = x + step

solve1 :: Int -> String -> Array Int
solve1 start input = 
  scanl moveStr start $ split (Pattern "\n") input

main :: forall eff. Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = do
  input <- readTextFile UTF8 "./src/Day2.in"
  log $ show $ solve1 5 input