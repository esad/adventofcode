module Day4P2 where

import Prelude
import Data.List
import Data.Tuple
import Data.Maybe
import Data.Array as A
import Data.String (split, Pattern(..), fromCharArray)
import Data.Either
import Data.Foldable
import Data.Generic
import Data.Char
import Control.Apply
import Text.Parsing.Simple (Parser, parse, many, char, lower, int, sepBy, letter, (<<), (>>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Sync (readTextFile)

decryptRoom :: String -> Maybe (Tuple Int String)
decryptRoom =
  parse (makeRoom <$> letters <*> int) >>> either (const Nothing) Just
  where
  letters = concat <$> many ((many lower) << char '-')
  makeRoom letters number = 
    letters # map (\x -> fromCharCode (97 + ((toCharCode x - 97 + number) `mod` 26)))
    # A.fromFoldable # fromCharArray
    # Tuple number

main :: forall eff. Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = do
  lines <- readTextFile UTF8 "./src/Day4.in"
  traverse_ (log <<< show) $ split (Pattern "\n") lines # map decryptRoom
