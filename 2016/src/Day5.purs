module Day5 where

import Base
import Data.String as String
import Data.Char as Char
import Data.List as List
import Data.List ((:), List(..))
import Data.Array as Array
import Helpers (md5)

findPassword :: String -> String
findPassword doorId =
  aux 0 Nil
  where
    aux :: Int -> List Char -> String
    aux k cs =
      case doorId <> show k |> md5 |> String.toCharArray |> List.fromFoldable of
        '0':'0':'0':'0':'0':c:_ ->
          let result = (c : cs) in
          if List.length result == 8 then
            result |> Array.fromFoldable |> Array.reverse |> String.fromCharArray
          else
            aux (k+1) result
        _ ->
          aux (k+1) cs

findFancyPassword :: String -> String
findFancyPassword doorId =
  aux 0 (Array.replicate 8 '.')
  where
    aux :: Int -> Array Char -> String
    aux k cs =
      case doorId <> show k |> md5 |> String.toCharArray |> List.fromFoldable of
        '0':'0':'0':'0':'0':i:c:_ ->
          let
            idx = (Char.toCharCode i) - (Char.toCharCode '0')
            result = fromMaybe cs $ Array.modifyAt idx (\x -> if x == '.' then c else x) cs
          in
          if Array.elemIndex '.' result == Nothing then
            String.fromCharArray result
          else
            aux (k+1) result
        _ ->
          aux (k+1) cs

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log $ show $ findPassword "wtnhxymk"
  log $ show $ findFancyPassword "wtnhxymk"
  -- log $ show $ findFancyPassword "abc"
