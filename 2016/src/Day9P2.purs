module Day9P2 where

import Base
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromNumber, toNumber)
import Data.String (takeWhile, take, drop, length)
import Data.Tuple (Tuple(..))
import Text.Parsing.Simple (unparser, char, int)

uncompress :: String -> Number
uncompress "" = toNumber 0
uncompress input =
  let
    result = unparser marker input
  in
    case result.consumed of
      Left _ ->
        let taken = length $ takeWhile (_ /= '(') input in
        toNumber taken + uncompress (drop taken result.remaining)
      Right (Tuple len times) ->
        let taken = uncompress $ take len result.remaining in
        taken * toNumber times + (uncompress $ drop len result.remaining)
  where
    marker = Tuple <$> (char '(' *> int) <*> (char 'x' *> int <* char ')')

main :: Main Unit
main = do
  lines <- inputLines "./src/Day9.in"
  log $ show $ sum $ uncompress <$> lines
