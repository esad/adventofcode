module Day10 where

import Base
import Data.Array (mapMaybe, length, sort, foldM)
import Data.Map (Map, insert, lookup, empty)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (product)
import Data.Traversable (sequence)
import Control.Alt ((<|>))
import Text.Parsing.Simple (parse, string, int)
import Partial.Unsafe (unsafeCrashWith)
import Control.Monad.Writer

data Addr = Bot Int | Output Int
type Bot = { high :: Addr, low :: Addr, holding :: Maybe Int }
type State = { bots :: Map Int Bot, outputs :: Map Int Int }
data Command = Setup Int {high :: Addr, low :: Addr } | Feed Addr Int

instance showAddr :: Show Addr where
  show (Bot id) = "(Bot #" <> show id <>")"
  show (Output id) = "(Output #" <> show id <>")"
derive instance eqAddr :: Eq Addr

instance showCommand :: Show Command where
  show (Feed addr value) = show addr <> "->" <> show value
  show (Setup id {high, low}) = "Bot #" <> show id <> " HI: " <> show high <> "LO:" <> show low
derive instance eqCommand :: Eq Command

-- Order commands to that setup is executed first
instance ordCommand :: Ord Command where
  compare (Setup _ _) (Feed _ _) = LT
  compare (Feed _ _) (Setup _ _) = GT
  compare _ _ = EQ

parseCommand :: String -> Maybe Command
parseCommand =
  (parse $ setupParser <|> feedParser) >>> eitherToMaybe
  where
    -- bot 181 gives low to bot 144 and high to bot 101
    setupParser =
      Setup <$> (string "bot " *> int) <*>
        ({high: _, low: _} <$> (string " gives low to " *> addrParser) <*> (string " and high to " *> addrParser))
    -- value 23 goes to bot 68
    feedParser =
      (flip Feed) <$> (string "value " *> int) <*> (string " goes to " *> addrParser)
    addrParser =
      (Bot <$> (string "bot " *> int)) <|> (Output <$> (string "output " *> int))

eval :: Command -> State -> Writer (Array String) State
eval (Feed (Output id) value) state =
  pure $ state { outputs = insert id value state.outputs }
eval (Feed (Bot id) value) state =
  case lookup id state.bots of
    Nothing ->
      unsafeCrashWith $ "Feed into unknown bot " <> show id
    Just bot@{ holding : Nothing } ->
      pure $ state { bots = insert id (bot { holding = Just value }) state.bots }
    Just bot@{ holding : Just value', high, low} ->
      let
        minmax = if value > value' then Tuple value value' else Tuple value' value
        min = fst minmax
        max = snd minmax
      in do
        tell $ ["Bot " <> show id <> " comparing " <> show min <> "/" <> show max]
        eval (Feed low min) state >>= eval (Feed high max)
eval (Setup id {low, high}) state =
  pure $ state { bots = insert id {holding: Nothing, low: low, high: high} state.bots }

main :: Main Unit
main = do
  lines <- inputLines "./src/Day10.in"
  let
    result = mapMaybe parseCommand lines |> sort |> foldM (flip eval) emptyState |> runWriter
    requestedOutputValues = (flip lookup) (fst result |> _.outputs) <$> [0,1,2]
  log $ show $ snd result -- dump log, then grep for "Bot ... comparing 61/17"
  log $ show $ map product (sequence requestedOutputValues) -- dumps the product of results of output 0,1,2
  where
    emptyState = { bots: empty, outputs: empty }
