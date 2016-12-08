module Day5 where

import Debug.Trace
import Base
import Data.String as String
import Data.List as List
import Data.List ((:), List(..))
import Data.Array as Array
import Helpers

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

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log $ show $ findPassword "wtnhxymk"
