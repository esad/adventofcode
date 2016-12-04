module Day3 where

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

parseTriangle :: String -> Either String Triangle
parseTriangle =
  parse $ (Triangle <$> number <*> number <*> number) `suchThat` valid
  where
    number = (skipSpaces *> int)
    valid (Triangle x y z) | x + y <= z = false
    valid (Triangle x y z) | x + z <= y = false
    valid (Triangle x y z) | y + z <= x = false
    valid (Triangle x y z) | otherwise = true


main :: forall eff. Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = 
  readTextFile UTF8 "./src/Day3.in"
    >>= split (Pattern "\n") 
    >>> fromFoldable 
    >>> mapMaybe (parseTriangle >>> (either (const Nothing) Just))
    >>> length 
    >>> show 
    >>> log