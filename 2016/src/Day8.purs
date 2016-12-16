module Day8 where

import Base
import Data.Array (drop, filter, length, mapWithIndex, modifyAt, replicate, reverse, slice, take, (!!), (..))
import Data.Foldable (foldl)
import Data.String (fromCharArray)
import Data.Traversable (traverse_)
import Text.Parsing.Combinators (choice)
import Text.Parsing.Simple (parse, string, int)

data Command
  = Rect { w :: Int, h :: Int}
  | RotR { y :: Int, by :: Int }
  | RotC { x :: Int, by :: Int }

type Screen = {w :: Int, h :: Int, pixels :: Array Boolean}

parseCommand :: String -> Command
parseCommand =
  compose forceRight $ parse $ choice
    [ { w: _, h: _} <$> (string "rect " *> int) <*> (string "x" *> int) <#> Rect
    , { x: _, by: _ } <$> (string "rotate column x=" *> int) <*> (string " by " *> int) <#> RotC
    , { y: _, by: _ } <$> (string "rotate row y=" *> int) <*> (string " by " *> int) <#> RotR
    ]

init :: Int -> Int -> Screen
init w h =
  { w: w, h: h, pixels: replicate (w*h) false }

update :: Int -> Int -> (Boolean -> Boolean) -> Screen -> Screen
update x y u screen@{w,h,pixels} =
  screen { pixels = modifyAt (y * w + x) u pixels |> forceJust }

rotate :: forall a . Int -> Array a -> Array a
rotate n xs = let n' = n `mod` length xs in drop n' xs <> take n' xs

reverseRotate :: forall a . Int -> Array a -> Array a
reverseRotate n  =
  reverse >>> rotate n >>> reverse

eval :: Command -> Screen -> Screen
eval cmd screen@{w,h,pixels} =
  foldl compose id (updates cmd) screen
  where
    updates (Rect r) = do
      x <- 0..(r.w-1)
      y <- 0..(r.h-1)
      pure $ update x y (const true)
    updates (RotR {y, by}) =
      pixels |> slice (y*w) ((y+1)*w) |> reverseRotate by |> mapWithIndex \i p -> update i y (const p)
    updates (RotC {x, by}) =
      mapWithIndex (\i p -> update x i (const p)) $ reverseRotate by  do
        y <- 0..(h-1)
        pure $ (forceJust $ pixels !! (y * w + x))

render :: Screen -> Array String
render {w,h,pixels} = do
  y <- 0..(h-1)
  pure $ slice (y*w) ((y+1)*w) pixels |> map (\p -> if p then '█' else ' ') |> fromCharArray

main :: Main Unit
main = do
  lines <- inputLines "./src/Day8.in"
  let screen = foldl (flip eval) (init 50 6) (map parseCommand lines)
  log $ show $ screen |> (_.pixels >>> filter id >>> length)
  traverse_ log (render screen)
