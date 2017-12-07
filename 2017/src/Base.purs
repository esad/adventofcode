module Base (module Prelude, module E, readFile, forceJust, lines, parseInts) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
--import Control.Monad.Eff.Console (log)
import Prelude
import Data.List ((:), List(..)) as E
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..), fromMaybe) as E

import Control.Monad.Aff.Console (log) as E
import Test.Unit (test, suite) as E 
import Test.Unit.Main (runTest) as E
import Test.Unit.Assert (equal) as E

import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))

import Partial.Unsafe (unsafeCrashWith)
import Data.Array (filter, catMaybes)
import Data.String (split, Pattern(..), null)
import Data.Int (fromString)

import Debug.Trace (spy) as E

readFile = FS.readTextFile UTF8

forceJust :: forall a . Maybe a -> a
forceJust Nothing = unsafeCrashWith "Can't extract just value"
forceJust (Just x) = x

lines :: String -> Array String
lines = split (Pattern "\n") >>> filter (not null)

parseInts :: String -> Array Int
parseInts = lines >>> map fromString >>> catMaybes
