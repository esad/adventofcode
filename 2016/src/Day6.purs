module Day6 where

import Base
import Data.Tuple
import Data.Profunctor.Strong
import Data.Array as A
import Data.List.NonEmpty as NEL
import Data.List (foldM, fromFoldable, group, head, sort, sortBy, transpose, last)
import Data.String (fromCharArray, toCharArray)

main :: Main Unit
main = do
  transposedLines <- (fromFoldable >>> map (toCharArray >>> fromFoldable) >>> transpose) <$> inputLines "./src/Day6.in"
  log $ code (-1 * _) transposedLines
  log $ code id transposedLines
  where
    code cmp lines = strFromMaybeChars $ (freqs >>> sortBy (comparing (snd >>> cmp)) >>> head >>> map fst) <$> lines
    strFromMaybeChars = A.fromFoldable >>> A.catMaybes >>> fromCharArray
    freqs = sort >>> group >>> map (NEL.head &&& NEL.length)
