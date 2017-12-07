module Day07 where

import Data.Either (hush)
import Text.Parsing.StringParser (runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1, between, option)
import Text.Parsing.StringParser.String (string, anyDigit, lowerCaseChar, whiteSpace)
import Data.Array (fromFoldable, catMaybes, groupBy)
import Data.Int (fromString)
import Data.String (fromCharArray)
import Data.Map (Map, empty, alter, lookup)
import Data.Foldable (foldl, find, sum, length)
import Data.Maybe (isNothing)
import Data.Function (on)
import Data.NonEmpty (head)
import Data.Ord (abs)
import Base

type ProgramDef = { name :: String, children :: Array String, weight :: Int}

newtype Node = Node { name :: String, children :: Array Node, weight :: Int }

readWeight  :: Node -> Int
readWeight (Node {weight}) = weight

instance showNode :: Show Node where
  show (Node {name, children, weight}) = name <> "(" <> show weight <> ") " <> show children 

parse :: String -> Maybe ProgramDef
parse =
  hush <<< runParser do
    name <- nameP
    _ <- whiteSpace
    weight <- parens int
    children <- option [] childrenP
    pure { name, children, weight }
  where
    parens = between (string "(") (string ")")
    int = (forceJust <<< fromString <<< fromCharArray <<< fromFoldable) <$> many1 anyDigit
    nameP = (fromCharArray <<< fromFoldable) <$> many1 lowerCaseChar
    childrenP = do
      _ <- string " -> "
      fromFoldable <$> sepBy1 nameP (string ", ")

type Index = Map String { parent :: Maybe String, def :: ProgramDef }

index :: String -> Index
index =
  lines >>> map parse >>> catMaybes >>> (\xs -> foldl fillParent (foldl fillDef empty xs) xs)
  where
    fillDef :: Index -> ProgramDef -> Index
    fillDef m def@{name} =
      alter (const $ Just {def, parent: Nothing}) name m
    fillParent :: Index -> ProgramDef -> Index
    fillParent m def@{name, children} =
      foldl (\m' child -> alter (map $ _ { parent = Just name}) child m') m children

makeNode :: Index -> String -> Node
makeNode index name =
  let
    {def: {weight, children}} = lookup name index # forceJust
    childrenNodes = children <#> makeNode index
    childrenWeight = sum $ childrenNodes <#> readWeight
  in
    Node {name, children: childrenNodes, weight: weight + childrenWeight }

root :: Index -> String
root = find (\{parent} -> isNothing parent) >>> (map $ _.def.name) >>> forceJust

solve1 :: String -> String
solve1 = index >>> root

solve2 :: String -> Maybe Int
solve2 input =
  abs <$> diff (makeNode i (root i)) 0
  where
    i = index input
    diff :: Node -> Int -> Maybe Int
    diff (Node {children: []}) d = Just d
    diff (Node {children: [a], weight }) d = diff a (d-weight)
    diff (Node {children: [a,b], weight }) d = 
      let
        wa = readWeight a
        wb = readWeight b
      in if wa == wb then Just (d - weight) else diff a wb
    diff (Node {children, weight, name}) d =
      let
        groups = groupBy ((==) `on` readWeight) children
        balanced = find (\xs -> length xs > 1) groups # forceJust # head
      in
        case find (\xs -> length xs == 1) groups of
          Nothing ->
            -- no unbalanced children, this node should be adjusted
            Just $ d - (sum $ readWeight <$> children)
          Just unbalanced ->
            diff (head unbalanced) (readWeight balanced)

main = runTest do
  suite "Day7" do
    test "part 1" do
      equal "tknk" $ solve1 sample
      equal (Just 60) $ solve2 sample
    test "input" do
      input <- readFile "./inputs/Day07.txt"
      log $ show $ solve1 input
      log $ show $ solve2 input
  where
    sample = """
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"""
