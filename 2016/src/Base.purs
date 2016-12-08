module Base (module Prelude, (|>), module Control.Monad.Eff, module Control.Monad.Eff.Console, module Data.Maybe) where

import Prelude
import Data.Function (applyFlipped)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe

infixl 1 applyFlipped as |>
