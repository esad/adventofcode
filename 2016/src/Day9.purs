module Day9 where

import Base
import Data.Tuple (Tuple(..))
import Data.String (takeWhile, take, drop, length)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Text.Parsing.Simple (unparser, char, int)

uncompress :: String -> Int
uncompress "" = 0
uncompress input =
  let
    result = unparser marker input
    tl = case result.consumed of
      Left _ ->
        {taken: takeWhile (_ /= '(') input |> length, times: 1}
      Right (Tuple len times) ->
        {taken: take len result.remaining |> length, times: times}
  in
    tl.taken * tl.times + (uncompress $ drop tl.taken result.remaining)
  where
    marker = Tuple <$> (char '(' *> int) <*> (char 'x' *> int <* char ')')

main :: Main Unit
main = do
  lines <- inputLines "./src/Day9.in"
  log $ show $ sum $ uncompress <$> lines
