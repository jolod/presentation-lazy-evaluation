module Main where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.PQueue.Min as PQueue
import Data.PQueue.Min (MinQueue)
import qualified Control.Applicative as Alternative
import Control.Applicative (Alternative, (<|>))

-- | Logical involutive negation, i.e. negation that satisfies the law of double negation.
-- https://en.wikipedia.org/wiki/Involution_(mathematics)#Mathematical_logic
class Ord a => Inv a where
  inv :: a -> a

-- | Negation is involutive on real numbers except for zero, so don't use `0` as a literal.
instance Inv Int where
  inv x = -x

-- | Instead of a `Set`, use a `MinQueue` where the set is "decorated" by the size.
-- That gives O(1) access to the smallest set, which gives fast access to singleton sets (i.e. unit clauses).
type Disjs a = PQueue.MinQueue (Int, Set a)

-- | CNF = Clausal normal form.
-- https://en.wikipedia.org/wiki/Conjunctive_normal_form
data CNF a = CNF (Disjs a)
  deriving Show

setWithSize set = (Set.size set, set)

cnfFromClauses :: Ord a => [[a]] -> CNF a
cnfFromClauses clauses = CNF $ PQueue.fromList $ map (setWithSize . Set.fromList) $ clauses

data Reduction a = Contradiction
                 | NoUnits
                 | Unit a (CNF a)

reduce :: Inv a => CNF a -> Reduction a
reduce (CNF disjs) =
    case PQueue.getMin disjs of
      Nothing -> NoUnits
      Just (_, lits) ->
        case Set.toList lits of
          [] -> Contradiction
          [x] -> Unit x (CNF $ PQueue.mapMaybe (reduceLits x) disjs)
          _ -> NoUnits
  where
    reduceLits x disj@(_, lits) =
      if Set.member x lits then
        Nothing
      else if Set.member (inv x) lits then
        Just $ setWithSize $ Set.delete (inv x) lits
      else
        Just disj

branch :: Inv a => CNF a -> Maybe (CNF a, CNF a)
branch (CNF disjs) =
    case PQueue.getMin disjs of
      Nothing -> Nothing
      Just (_, lits) ->
        case Set.toList lits of
          [] -> Nothing
          (x:_) -> Just (add x disjs, add (inv x) disjs)
  where
    add x = CNF . PQueue.insert (1, Set.singleton x)

mkSolver cnf = Solver { units = []
                      , cnf = cnf }

data Solver a = Solver
  { units :: [a]
  , cnf :: CNF a }
  deriving Show

solve :: Inv a => Solver a -> [[a]]
solve solver =
  case reduce (cnf solver) of
    Contradiction -> -- Contradiction.
      mempty
    Unit x cnf -> -- Reduction performed, repeat.
      solve Solver { units = x : units solver
                   , cnf = cnf }
    NoUnits -> -- No reduction performed, attempt to branch.
      case branch (cnf solver) of
        Nothing -> -- Nothing to branch on; solution found.
          pure $ units solver
        Just (left, right) -> -- Explore both branches.
          solve solver {cnf = left} <> solve solver {cnf = right}

aSolution :: Inv a => Solver a -> Maybe [a]
aSolution = head' . solve
  where
  head' [] = Nothing
  head' (x:_) = Just x

-- In a non-lazy language you'll have to write something like this.
-- It's exactly like `solve`, but the last line of `solve` is
-- replaced by a case.
aSolution' :: Inv a => Solver a -> Maybe [a]
aSolution' solver =
  case reduce (cnf solver) of
    Contradiction -> -- Contradiction.
      mempty
    Unit x cnf -> -- Reduction performed, repeat.
      aSolution' Solver { units = x : units solver
                        , cnf = cnf }
    NoUnits -> -- No reduction performed, attempt to branch.
      case branch (cnf solver) of
        Nothing -> -- Nothing to branch on; solution found.
          pure $ units solver
        Just (left, right) -> -- Explore both branches.
          case aSolution' solver {cnf = left} of
            Nothing ->
              aSolution' solver {cnf = right}
            Just solution ->
              Just solution

-- | Version of `solve` using `Alternative`.
solve' :: (Ord a, Inv a, Alternative f) => Solver a -> f [a]
solve' solver =
  case reduce (cnf solver) of
    Contradiction -> -- Contradiction.
      Alternative.empty
    Unit x cnf -> -- Reduction performed, repeat.
      solve' Solver { units = x : units solver
                   , cnf = cnf }
    NoUnits -> -- No reduction performed, attempt to branch.
      case branch (cnf solver) of
        Nothing -> -- Nothing to branch on; solution found.
          pure $ units solver
        Just (left, right) -> -- Explore both branches.
          solve' solver {cnf = left} <|> solve' solver {cnf = right}

main :: IO ()
main =
  let
    -- We need to add types because `show` needs to know what type `a` is.
    allSolutions :: [[Int]]
    allSolutions = solve $ mkSolver $ cnfFromClauses [[1], [1,2], [-1,3], [2,4]]

    firstSolution :: Maybe [Int]
    firstSolution = aSolution $ mkSolver $ cnfFromClauses [[1], [1,2], [-1,3], [2,4]]

    -- For the `Alternative` version we need to also specify which functor to use.
    allSolutionsAlternative :: [[Int]]
    allSolutionsAlternative = solve' $ mkSolver $ cnfFromClauses [[1], [1,2], [-1,3], [2,4]]

    firstSolutionAlternative :: Maybe [Int]
    firstSolutionAlternative = solve' $ mkSolver $ cnfFromClauses [[1], [1,2], [-1,3], [2,4]]
  in do
    putStrLn $ show allSolutions
    putStrLn $ show firstSolution
    putStrLn $ show allSolutionsAlternative
    putStrLn $ show firstSolutionAlternative
