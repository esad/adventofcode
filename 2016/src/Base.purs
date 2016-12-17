module Base (module Prelude, (|>), module Control.Monad.Eff, module Control.Monad.Eff.Console, module Data.Maybe, inputLines, eitherToMaybe, forceRight, forceJust, Main, debug) where

import Prelude
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..), either)
import Data.Function (applyFlipped)
import Data.String (split, Pattern(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith)
import Debug.Trace (trace)

infixl 1 applyFlipped as |>

eitherToMaybe :: forall a b . Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

forceRight :: forall a b . Either a b -> b
forceRight (Left _) = unsafeCrashWith "Can't extract right value"
forceRight (Right x) = x

forceJust :: forall a . Maybe a -> a
forceJust Nothing = unsafeCrashWith "Can't extract just value"
forceJust (Just x) = x

type Main a = forall eff . Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) a

inputLines :: forall eff . String -> Eff (fs :: FS, err :: EXCEPTION | eff) (Array String)
inputLines path =
  split (Pattern "\n") <$> readTextFile UTF8 path

debug :: forall a. Show a => String -> a -> a
debug str a =
  trace (str <> show a) (const a)
