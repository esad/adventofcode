module Day4 where

import Prelude
import Data.List
import Data.Tuple
import Data.Maybe
import Data.String (split, Pattern(..))
import Data.Profunctor.Strong
import Data.List.NonEmpty as NEL
import Data.Either
import Data.Foldable
import Data.Generic
import Control.Apply
import Text.Parsing.Simple (Parser, parse, many, char, lower, int, sepBy, letter, (<<), (>>))
import Control.MonadZero (guard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Sync (readTextFile)


data Room = Room (List (Tuple Char Int)) Int (List Char)

derive instance genericRoom :: Generic Room
instance showRoom :: Show Room where show = gShow

number (Room _ n _) = n
valid (Room freqs _ cs) = take (length cs) (map fst freqs) == cs

parseRoom :: String -> Maybe Room
parseRoom =
  parse ((freqs >>> Room) <$> letters <*> int <*> (char '[' >> (many lower) << char ']')) >>> either (const Nothing) Just
  where
  letters = concat <$> many ((many lower) << char '-')
  freqs = sort >>> group >>> map (NEL.head &&& NEL.length) >>> sortBy (comparing (snd >>> ((*)(-1))))

main :: forall eff. Eff (fs :: FS, err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = do
  lines <- readTextFile UTF8 "./src/Day4.in"
  log $ show $ lines # split (Pattern "\n") # fromFoldable # mapMaybe parse # sum
  where
    parse line = do
      room <- parseRoom line
      guard $ valid room
      pure $ number room
