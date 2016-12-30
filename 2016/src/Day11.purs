module Day11 where

import Base
import Data.Set as S
import Control.Monad.Rec.Class (tailRec, Step(..))
import Control.MonadPlus (guard)
import Data.Array (all, concat, concatMap, foldl, fromFoldable, insertBy, intersect, mapMaybe, mapWithIndex, modifyAt, nub, null, reverse, sort, uncons, (..))
import Data.Foldable (sum)
import Data.String (joinWith, take)

topFloor :: Int
topFloor = 3

finalFloor :: Int
finalFloor = topFloor

newtype Pair = Pair { chip :: Int, rtg :: Int, name :: String }
newtype State = State { elevator :: Int, pairs :: Array Pair }

instance eqPair :: Eq Pair where
  eq (Pair p1) (Pair p2) = p1.name == p2.name && p1.chip == p2.chip && p1.rtg == p2.rtg

instance ordPair :: Ord Pair where
    compare (Pair p1) (Pair p2) = compare p1.name p2.name <> compare p1.chip p2.chip <> compare p1.rtg p2.rtg

instance eqState :: Eq State where
  eq (State s1) (State s2) =
    s1.elevator == s2.elevator && sums s1.pairs == sums s2.pairs
    where
      sums pairs = sort $ (\(Pair {chip, rtg}) -> topFloor * rtg + chip) <$> pairs

instance ordState :: Ord State where
  compare (State s1) (State s2) = compare s1.elevator s2.elevator <> compare s1.pairs s2.pairs

instance showState :: Show State where
  show (State {elevator, pairs}) =
    joinWith "\n" (reverse lines) <> "\n"
    where
      objects i =
        pairs |> concatMap \(Pair {chip, rtg, name}) ->
          (if chip == i then [take 2 name <> "M"] else []) <>
          if rtg == i then [take 2 name <> "G"] else []
      lines = do
        f <- 0 .. topFloor
        pure $ "F" <> show f <> (if elevator == f then " E " else " . ") <> joinWith " " (objects f)

input :: State
input =
  State
    { elevator : 0
    , pairs :
      [ Pair { chip: 1, rtg: 0, name: "Polonium"}
      , Pair { chip: 0, rtg: 0, name: "Thulium"}
      , Pair { chip: 0, rtg: 0, name: "Cobalt"}
      , Pair { chip: 0, rtg: 0, name: "Rhutenium"}
      , Pair { chip: 1, rtg: 0, name: "Promethium"}
      ]
    }

input2 :: State
input2 =
  (\(State s) -> State $ s { pairs = s.pairs <> [Pair { chip: 0, rtg: 0, name: "Elerium"}, Pair { chip: 0, rtg: 0, name: "Dilithium"}]}) input

data Move = Chip Int | RTG Int
derive instance eqMove :: Eq Move
derive instance ordMove :: Ord Move

validMoves :: Array Move -> Array (Array Move)
validMoves allMoves = do
  m1 <- allMoves
  one <- [false, true]
  if one
    then pure [m1]
    else do
      m2 <- allMoves
      guard $ m1 < m2 && compatible m1 m2
      pure [m1, m2]
  where
    compatible (Chip x) (RTG y) = x == y
    compatible _ _ = true

applyMove :: Int -> State -> Move -> State
applyMove floor (State {elevator, pairs}) (Chip i) =
  State {elevator: floor, pairs: modifyAt i (\(Pair p) -> Pair $ p { chip = floor }) pairs |> forceJust}
applyMove floor (State {elevator, pairs}) (RTG i) =
  State {elevator: floor, pairs: modifyAt i (\(Pair p) -> Pair $ p { rtg = floor }) pairs |> forceJust}

branches :: State -> Array State
branches (state@State {elevator, pairs}) = do
  e' <- (elevator + _) <$> [1, -1]
  guard $ between 0 topFloor e'
  let allMoves = (validMoves <<< concat <<< mapWithIndex (\i (Pair {rtg, chip}) ->
      (if rtg == elevator then [RTG i] else []) <> (if chip == elevator then [Chip i] else [])
    )) pairs
  moves <- allMoves
  let result = foldl (applyMove e') state moves
  guard $ safe result
  pure result

final :: State -> Boolean
final (State {elevator, pairs}) =
  elevator == finalFloor && all (\(Pair {chip, rtg}) -> chip == finalFloor && chip == rtg) pairs

safe :: State -> Boolean
safe (State {pairs}) =
  null $ intersect unshielded rtgs
  where
    unshielded = mapMaybe unpairedChip pairs
    unpairedChip (Pair {chip, rtg}) = if chip /= rtg then Just chip else Nothing
    rtgs = nub $ map (\(Pair {rtg}) -> rtg) pairs

type Edge = { state :: State, moves :: Int }

solution :: State -> Maybe Edge
solution i =
  tailRec  search { visited: S.empty, toVisit: [{ state: i, moves: 0 }] }
  where
    search {visited, toVisit} =
      case uncons toVisit of
        Just { head: e@{state, moves}, tail } ->
          if final state then
            Done $ Just e
          else
            let
              new = S.fromFoldable $ branches state
              visited' = S.insert state visited
            in
              Loop
                { visited: visited'
                , toVisit: foldl (flip $ insertBy $ compareResult) tail ({state: _, moves: moves+1 } <$> (fromFoldable $ S.difference new visited'))
                }
        _ ->
          Done Nothing
    compareResult r1 r2 = compare (cost r1) (cost r2) <> compare r1.moves r2.moves
    cost { state: State {pairs} } =
      sum $ map (\(Pair {chip,rtg}) -> 2*topFloor - rtg - chip) pairs

main :: Main Unit
main = do
  log $ show $ _.moves <$> solution input
  log $ show $ _.moves <$> solution input2
