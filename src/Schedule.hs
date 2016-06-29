{-# LANGUAGE
MultiWayIf,
RecordWildCards,
ScopedTypeVariables,
TemplateHaskell,
NoImplicitPrelude
  #-}


module Schedule
(
  Schedule,
  PartialSchedule(..),
    schPastGames,
    schPlayerCount,
    schCurrent,
    schBest,
    schIterationsTotal,
    schIterationsLeft,
  randomSchedule,
  advancePartialSchedule,
  precomputedSchedules,
)
where


-- General
import BasePrelude
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Lists
import Data.List.Index
-- Containers
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as U
-- Random
import System.Random.Shuffle
import Control.Monad.Random
-- acid-state
import Data.SafeCopy


type Schedule = U.Vector (Int, Int)

data PartialSchedule = PartialSchedule {
  _schPlayerCount     :: Int,
  _schPastGames       :: U.Vector (Int, Int),
  _schCurrent         :: Schedule,
  _schBest            :: Schedule,
  _schIterationsTotal :: Int,
  _schIterationsLeft  :: Int }
  deriving (Eq, Show)

deriveSafeCopySimple 0 'base ''PartialSchedule
makeLenses ''PartialSchedule

advancePartialSchedule
  :: Int -> PartialSchedule -> IO (Either PartialSchedule Schedule)
advancePartialSchedule n p@PartialSchedule{..}
  | _schIterationsLeft <= 0 = return (Right _schBest)
  | otherwise = do
      let curIter = _schIterationsTotal - _schIterationsLeft
          iters   = min n _schIterationsLeft
      (cur', bst') <- iterateSchedule
                        _schPlayerCount _schPastGames
                        curIter iters
                        (_schCurrent, _schBest)
      return $ Left p{
        _schCurrent = cur',
        _schBest = bst',
        _schIterationsLeft = _schIterationsLeft - iters }

{- |
Things influencing rating:

* We want people to have good minimum time between rounds
* We want to avoid the situation when the person plays in the same role
  (word-namer / word-guesser) several times in a row

Smaller score = better.
-}
rateSchedule
  :: Int                  -- ^ Player count
  -> U.Vector (Int, Int)  -- ^ Past games
  -> Schedule             -- ^ Solution
  -> Double
rateSchedule pc past = do
  -- We store the following information about each player:
  --   * time of last played round
  --   * time of last played round as namer
  --   * time of last played round as guesser
  --   * the player they last played against (to prevent pairs like
  --     (x, y), (y, x) from occurring in a row)
  --   * minimum time between rounds
  --   * minimum time between same-role rounds in a row
  --   * minimum time between swapped pairs
  let update turn isNamer opponent
        (lastTurn, lastNamer, lastGuesser, prevOpponent,
         minRounds, minRoundsSame, minSwapped)
        =
        (turn,
         if isNamer then turn else lastNamer,
         if isNamer then lastGuesser else turn,
         opponent,
         if lastTurn /= -1
           then min minRounds (turn-lastTurn)
           else minRounds,
         -- if both are -1 and the player hasn't played before, none of
         -- these branches will fire because -1 isn't greater than -1
         if | isNamer && lastNamer > lastGuesser ->
                min minRoundsSame (turn-lastNamer)
            | not isNamer && lastGuesser > lastNamer ->
                min minRoundsSame (turn-lastGuesser)
            | otherwise ->
                minRoundsSame,
         if opponent == prevOpponent
           then min minSwapped (turn-lastTurn)
           else minSwapped)
  -- Now we calculate all that info for past games.
  let initTuple = (-1, -1, -1, -1, 1000000, 1000000, 1000000)
  let initMap = IM.fromList [(i, initTuple) | i <- [0..pc-1]]
  let updateMap :: Int
                -> IM.IntMap (Int, Int, Int, Int, Int, Int, Int)
                -> U.Vector (Int, Int)
                -> IM.IntMap (Int, Int, Int, Int, Int, Int, Int)
      updateMap sh =
        U.ifoldl (\mp turn (a, b) -> mp & ix a %~ update (sh+turn) True  b
                                        & ix b %~ update (sh+turn) False a)
  let pastMap = updateMap 0 initMap past
  let rate (_, _, _, _, minRounds, minRoundsSame, minSwapped) =
        (case minSwapped of
           1 -> 5
           2 -> 4
           _ -> 1 / (1 + fromIntegral minSwapped)) +
        (case minRounds of
           1 -> 2
           _ -> 1 / fromIntegral minRounds) +
        (case minRoundsSame of
           1 -> 3
           2 -> 1.5
           _ -> 1 / fromIntegral minRoundsSame)
  (\future -> sum $ imap (\i x -> x*0.5^(pc-i-1)) $ sort $
              map rate $ IM.elems $ updateMap (U.length past) pastMap future)

-- | Generate a random future schedule (given past games).
randomSchedule :: Int -> U.Vector (Int, Int) -> IO Schedule
randomSchedule pc past =
  fmap U.fromList $ shuffleM $
  [(x, y) | x <- [0..pc-1], y <- [0..pc-1], x/=y] \\ U.toList past

-- https://en.wikipedia.org/wiki/Simulated_annealing
iterateSchedule
  :: Int                       -- ^ Player count
  -> U.Vector (Int, Int)       -- ^ Past games
  -> Int                       -- ^ Current iteration
  -> Int                       -- ^ Iterations to do
  -> (Schedule, Schedule)      -- ^ Current schedule, best schedule
  -> IO (Schedule, Schedule)   -- ^ New current and best schedule
iterateSchedule pc past kcur kn (cur, bst)
  | U.null cur = return (cur, bst)
  | otherwise  = go (cur, rate cur) (bst, rate bst) kcur kn
  where
    rate = rateSchedule pc past
    p e e' t = if e' < e then 1 else exp ((e-e')/t)
    go (s, _)  (sbest, _)      _ 0 = return (s, sbest)
    go (s, rs) (sbest, rsbest) k n = do
      s' <- swap2 s
      let t = 0.99999**(fromIntegral k)
          rs' = rate s'
      rnd <- randomIO
      let (sbest', rsbest')
            | rs' < rsbest = (s', rs')
            | otherwise    = (sbest, rsbest)
      if p rs rs' t >= rnd
        then go (s', rs') (sbest', rsbest') (k+1) (n-1)
        else go (s , rs ) (sbest', rsbest') (k+1) (n-1)

-- | Swap 2 random elements of an array.
swap2 :: U.Unbox a => U.Vector a -> IO (U.Vector a)
swap2 xs = do
  let len = U.length xs
  i <- randomRIO (0, len-1)
  j <- randomRIO (0, len-1)
  return (U.unsafeUpd xs [(i, U.unsafeIndex xs j), (j, U.unsafeIndex xs i)])

-- | Good solutions for group sizes from 4 to 7 (found by running simulated
-- annealing several times and choosing good solutions manually).
precomputedSchedules :: Map Int [U.Vector (Int, Int)]
precomputedSchedules =
  over (each.each) U.fromList $
  M.fromList $ zip [1..7]
    [schedule1, schedule2, schedule3, schedule4,
     schedule5, schedule6, schedule7]

schedule1 :: [[(Int, Int)]]
schedule1 = [[]]

schedule2 :: [[(Int, Int)]]
schedule2 = [
  [(0,1),(1,0)],
  [(1,0),(0,1)] ]

schedule3 :: [[(Int, Int)]]
schedule3 = [
  [(0,1),(2,0),(1,2),(0,2),(1,0),(2,1)],
  [(2,1),(1,0),(0,2),(1,2),(2,0),(0,1)] ]

schedule4 :: [[(Int, Int)]]
schedule4 = [
  [(3,2),(1,0),(0,3),(2,1),(3,0),(0,1),(2,3),(1,2),(2,0),(3,1),(0,2),(1,3)],
  [(0,3),(1,2),(3,1),(2,3),(1,0),(0,2),(3,0),(2,1),(1,3),(3,2),(0,1),(2,0)],
  [(2,3),(1,0),(3,1),(1,2),(0,3),(2,0),(3,2),(0,1),(1,3),(2,1),(3,0),(0,2)],
  [(3,2),(0,1),(1,3),(2,1),(3,0),(0,2),(2,3),(1,0),(3,1),(1,2),(0,3),(2,0)],
  [(2,0),(3,1),(0,3),(3,2),(1,0),(2,1),(0,2),(1,3),(3,0),(2,3),(0,1),(1,2)],
  [(2,3),(0,1),(3,0),(0,2),(1,3),(2,1),(1,0),(3,2),(0,3),(2,0),(3,1),(1,2)],
  [(2,0),(3,1),(0,3),(3,2),(1,0),(2,1),(1,3),(0,2),(3,0),(2,3),(0,1),(1,2)] ]

schedule5 :: [[(Int, Int)]]
schedule5 = [
  [(0,1),(2,3),(4,0),(1,2),(3,4),(2,0),(4,1),(3,2),(1,0),(4,3),(2,1),(3,0),(4,2),(1,3),(0,4),(3,1),(2,4),(0,3),(1,4),(0,2)],
  [(3,2),(1,0),(4,3),(2,1),(0,4),(1,3),(4,0),(1,2),(3,4),(0,1),(2,3),(1,4),(0,2),(3,1),(4,2),(3,0),(2,4),(0,3),(4,1),(2,0)],
  [(1,3),(0,2),(4,1),(3,0),(2,4),(1,0),(4,2),(0,3),(1,4),(3,2),(4,0),(2,1),(0,4),(2,3),(0,1),(3,4),(1,2),(4,3),(2,0),(3,1)],
  [(1,0),(4,3),(2,1),(3,0),(2,4),(0,1),(3,2),(0,4),(1,3),(4,2),(3,1),(0,2),(1,4),(0,3),(4,1),(2,0),(3,4),(1,2),(4,0),(2,3)],
  [(4,3),(1,0),(2,4),(0,3),(1,2),(4,0),(2,3),(0,1),(3,2),(4,1),(2,0),(3,1),(0,2),(1,4),(3,0),(4,2),(1,3),(0,4),(2,1),(3,4)],
  [(3,4),(0,2),(1,3),(2,4),(0,1),(4,3),(2,0),(3,1),(0,4),(1,2),(4,0),(2,1),(0,3),(4,2),(3,0),(1,4),(2,3),(4,1),(3,2),(1,0)] ]

schedule6 :: [[(Int, Int)]]
schedule6 = [
  [(3,4),(0,1),(2,5),(1,3),(5,0),(2,1),(4,3),(1,5),(0,4),(3,2),(4,1),(0,3),(5,4),(0,2),(3,1),(0,5),(2,4),(5,3),(1,2),(4,0),(2,3),(1,4),(3,0),(5,2),(1,0),(3,5),(4,2),(5,1),(2,0),(4,5)],
  [(2,4),(0,5),(1,3),(4,0),(3,2),(5,4),(0,2),(3,5),(4,1),(5,0),(2,3),(1,5),(0,4),(2,1),(4,3),(1,0),(2,5),(0,3),(5,1),(3,4),(1,2),(5,3),(4,2),(3,1),(2,0),(1,4),(5,2),(0,1),(4,5),(3,0)],
  [(1,0),(5,2),(3,4),(1,5),(2,0),(4,1),(5,0),(1,3),(4,2),(3,5),(0,4),(2,1),(0,3),(5,4),(3,1),(0,2),(4,3),(2,5),(1,4),(3,0),(4,5),(1,2),(5,3),(2,4),(0,1),(3,2),(4,0),(5,1),(2,3),(0,5)],
  [(1,2),(0,5),(3,4),(5,2),(4,1),(3,5),(2,4),(1,0),(3,2),(4,5),(1,3),(2,0),(5,1),(0,3),(4,2),(3,1),(5,0),(4,3),(2,1),(0,4),(1,5),(2,3),(5,4),(0,1),(2,5),(3,0),(1,4),(0,2),(5,3),(4,0)],
  [(5,3),(0,1),(2,4),(5,0),(4,1),(3,2),(1,0),(4,5),(3,1),(2,0),(4,3),(1,2),(0,4),(3,5),(2,1),(5,4),(3,0),(4,2),(1,3),(2,5),(4,0),(5,1),(0,2),(1,4),(2,3),(0,5),(3,4),(5,2),(0,3),(1,5)],
  [(2,1),(0,4),(5,3),(4,1),(2,0),(5,4),(1,3),(2,5),(3,4),(0,2),(1,5),(3,0),(4,2),(0,5),(3,1),(4,0),(2,3),(1,4),(3,5),(0,1),(2,4),(5,0),(1,2),(0,3),(4,5),(3,2),(5,1),(4,3),(1,0),(5,2)],
  [(4,1),(3,2),(0,5),(1,3),(2,4),(5,1),(4,3),(2,5),(3,0),(5,4),(0,1),(4,2),(1,5),(0,2),(3,1),(5,0),(2,3),(1,4),(3,5),(4,0),(2,1),(3,4),(1,0),(5,2),(0,3),(4,5),(1,2),(0,4),(5,3),(2,0)] ]

schedule7 :: [[(Int, Int)]]
schedule7 = [
  [(4,1),(2,6),(3,0),(1,5),(0,2),(6,4),(2,1),(5,0),(3,2),(0,1),(4,5),(3,6),(1,4),(2,5),(0,3),(6,2),(3,1),(5,6),(4,3),(2,0),(5,1),(0,4),(6,3),(5,2),(4,6),(1,3),(6,0),(5,4),(1,6),(2,3),(0,5),(4,2),(1,0),(5,3),(0,6),(3,4),(6,5),(4,0),(1,2),(3,5),(2,4),(6,1)],
  [(0,1),(5,4),(3,6),(0,2),(1,3),(6,0),(5,1),(2,6),(4,0),(5,2),(1,4),(3,0),(2,1),(4,3),(5,0),(6,4),(1,5),(0,6),(2,3),(6,1),(4,2),(5,6),(2,0),(3,4),(0,5),(6,2),(1,0),(4,6),(5,3),(2,4),(6,5),(3,2),(1,6),(2,5),(4,1),(0,3),(1,2),(3,5),(0,4),(6,3),(4,5),(3,1)],
  [(2,3),(5,4),(6,0),(1,2),(0,5),(3,6),(4,1),(6,2),(5,3),(1,6),(2,4),(0,1),(6,5),(4,0),(5,2),(0,6),(3,1),(2,0),(1,5),(0,3),(2,6),(3,5),(6,4),(1,0),(2,5),(4,3),(5,1),(3,2),(0,4),(5,6),(4,2),(6,1),(3,4),(5,0),(4,6),(0,2),(1,4),(6,3),(2,1),(3,0),(4,5),(1,3)],
  [(4,3),(2,0),(1,5),(6,3),(5,4),(0,1),(4,6),(3,0),(1,4),(5,2),(3,6),(2,1),(6,5),(4,2),(1,6),(0,4),(3,5),(6,0),(2,3),(5,1),(0,2),(1,3),(2,6),(4,5),(3,2),(5,0),(6,1),(3,4),(0,6),(2,5),(1,0),(5,3),(4,1),(6,2),(0,3),(2,4),(5,6),(1,2),(4,0),(3,1),(6,4),(0,5)],
  [(0,3),(5,4),(2,1),(3,6),(4,2),(6,5),(0,4),(5,3),(1,0),(4,6),(0,5),(3,4),(6,0),(1,3),(2,6),(4,1),(3,2),(6,4),(2,0),(4,3),(0,1),(6,2),(1,4),(2,5),(0,6),(5,1),(4,0),(3,5),(1,2),(5,6),(2,3),(4,5),(0,2),(1,6),(5,0),(3,1),(2,4),(6,3),(1,5),(3,0),(5,2),(6,1)],
  [(4,6),(5,0),(3,2),(6,1),(0,3),(2,4),(6,0),(1,2),(3,5),(2,6),(4,1),(5,2),(1,3),(0,5),(6,4),(3,0),(5,1),(2,3),(1,4),(6,5),(3,1),(5,4),(1,6),(0,2),(4,3),(2,5),(1,0),(6,2),(0,4),(2,1),(3,6),(4,2),(5,3),(0,6),(3,4),(2,0),(6,3),(4,5),(0,1),(5,6),(4,0),(1,5)],
  [(0,2),(5,4),(6,1),(2,3),(4,6),(3,5),(2,1),(6,3),(4,0),(5,6),(3,1),(6,2),(1,5),(0,6),(2,4),(5,0),(4,3),(6,5),(1,4),(3,2),(0,1),(4,5),(1,3),(5,2),(3,6),(4,1),(6,0),(1,2),(0,4),(5,3),(4,2),(1,0),(3,4),(2,5),(0,3),(1,6),(2,0),(5,1),(6,4),(0,5),(2,6),(3,0)],
  [(3,6),(4,1),(0,5),(2,6),(3,4),(1,2),(4,6),(3,0),(5,1),(6,3),(0,2),(4,5),(2,3),(0,6),(5,2),(1,4),(3,5),(4,0),(5,6),(2,4),(1,0),(4,3),(6,1),(0,4),(1,3),(6,5),(2,1),(5,0),(3,2),(1,5),(6,4),(2,0),(3,1),(6,2),(0,3),(5,4),(1,6),(2,5),(0,1),(4,2),(5,3),(6,0)],
  [(5,3),(4,0),(1,6),(3,2),(5,4),(6,0),(3,1),(2,4),(5,6),(0,2),(4,1),(3,5),(1,2),(4,6),(2,3),(0,1),(6,5),(4,3),(2,6),(0,5),(1,4),(2,0),(3,6),(5,1),(0,4),(1,3),(5,2),(6,4),(3,0),(1,5),(0,6),(4,2),(5,0),(6,3),(2,1),(4,5),(0,3),(6,2),(1,0),(3,4),(2,5),(6,1)] ]
