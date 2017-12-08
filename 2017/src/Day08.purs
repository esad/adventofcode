module Day08 where

import Data.Either (hush)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, between, choice, option)
import Text.Parsing.StringParser.String (string, anyDigit, lowerCaseChar, char)
import Data.Array (fromFoldable, catMaybes, length, last)
import Control.Bind (join)
import Data.Int (fromString)
import Data.String (fromCharArray)
import Control.Alt ((<|>))
import Data.Map (Map, empty, alter, lookup, values)
import Data.Foldable (foldl, maximum)
import Data.Traversable (Accum, mapAccumL)


import Base

data Op = Inc | Dec

data Rel = GT | GTE | LT | LTE | EQ | NE

data Instruction = Instruction String Op Int String Rel Int

parse :: String -> Maybe Instruction
parse =
  hush <<< runParser inst
  where
    inst :: Parser Instruction
    inst = Instruction <$> identifier <*> op <*> (int <* string " if ") <*> identifier <*> rel <*> int
    identifier = (fromCharArray <<< fromFoldable) <$> many1 lowerCaseChar
    op = between (string " ") (string " ") $ (string "inc" $> Inc) <|> (string "dec" $> Dec)
    int = option id (char '-' $> negate) <*> ((forceJust <<< fromString <<< fromCharArray <<< fromFoldable) <$> many1 anyDigit)
    rel = choice
      [ string " >= " $> GTE
      , string " > " $> GT
      , string " <= " $> LTE
      , string " < " $> LT
      , string " == " $> EQ
      , string " != " $> NE 
      ]

run :: Array Instruction -> Accum (Map String Int) (Array (Maybe Int))
run =
  mapAccumL eval empty
  where
    eval :: (Map String Int) -> Instruction -> Accum (Map String Int) (Maybe Int)
    eval env (Instruction reg op rval condReg condRel condVal) =
      let
        regVal = fromMaybe 0 $ lookup reg env
        regVal' = case op of
          Inc -> regVal + rval
          Dec -> regVal - rval
        val = fromMaybe 0 $ lookup condReg env
        cond = case condRel of
          GT -> val > condVal
          GTE -> val >= condVal
          LT -> val < condVal
          LTE -> val <= condVal
          EQ -> val == condVal
          NE -> val /= condVal
        accum = 
          if cond then
            alter (const $ Just regVal') reg env
          else
            env
      in
        { accum, value: accum # values # maximum }

solve1 :: String -> Maybe Int
solve1 = 
  lines >>> map parse >>> catMaybes >>> run >>> _.value >>> last >>> join

solve2 :: String -> Maybe Int
solve2 = 
  lines >>> map parse >>> catMaybes >>> run >>> _.value >>> maximum >>> join

main = runTest do
  suite "Day8" do
    test "part 1" do
      equal (Just 1) $ solve1 sample
      equal (Just 10) $ solve2 sample
    test "input" do
      input <- readFile "./inputs/Day08.txt"
      log $ show $ solve1 input
      log $ show $ solve2 input
  where
      sample = """
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
"""
