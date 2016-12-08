module Base (module Prelude, (|>), module Control.Monad.Eff, module Control.Monad.Eff.Console) where

import Prelude
import Data.Function (applyFlipped)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

infixl 1 applyFlipped as |>
