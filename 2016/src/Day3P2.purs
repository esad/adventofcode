module Day3P2 where

import Prelude
import Data.Tuple
import Control.Apply
import Data.Maybe
import Data.Either
import Data.List
import Data.String (split, Pattern(..))
import Text.Parsing.Simple (Parser, parse, skipSpaces, int, sepBy, suchThat)
import Control.Monad
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Sync (readTextFile)

data Triangle = Triangle Int Int Int
instance showTriangle :: Show Triangle where
  show (Triangle x y z) = show x <> "," <> show y <> "," <> show z

parseInput :: String -> List Triangle
parseInput =
  parse (skipSpaces *> int `sepBy` skipSpaces) >>> either (const Nil) id >>> triangles >>> filter valid
  where
    triangles (x1 : x2 : x3 : y1 : y2 : y3 : z1 : z2 : z3 : xs) =
      Triangle x1 y1 z1 : Triangle x2 y2 z2 : Triangle x3 y3 z3 : triangles xs
    triangles _ = Nil
    valid (Triangle x y z) | x + y <= z = false
    valid (Triangle x y z) | x + z <= y = false
    valid (Triangle x y z) | y + z <= x = false
    valid (Triangle x y z) | otherwise = true

main :: forall eff. Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = do
  lines <- readTextFile UTF8 "./src/Day3.in"
  log $ show $ length $ parseInput lines 
