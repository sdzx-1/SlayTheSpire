{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MyLib where

import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import Control.Monad (forM, forM_, forever, replicateM_, when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Vector hiding (forM, forM_, length, modify, zip, (++))
import Data.Vector.Mutable hiding (forM_, length, modify, read, readMaybe)
import System.Random (mkStdGen)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Type
import Utils

evalBehavior
  :: ( Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     )
  => Behavior
  -> m ()
evalBehavior b = do
  lift $ print b
  case b of
    Attack t v -> case t of
      P -> damagePlayer v
      E index -> damageEnemy index v
    Defend t v -> case t of
      P -> defendPlayer v
      E index -> defendEnemy index v

runF =
  runLabelledLift
    . runState initGameState
    . runState initPlayer
    . runState initEnemys
    . runRandom (mkStdGen 10)
    . runError @GameError
    . runError @InputControl
    $ f

f
  :: forall sig m
   . ( Has Random sig m
     , Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (State GameState) sig m
     , Has (Error GameError) sig m
     , Has (Error InputControl) sig m
     , HasLabelledLift IO sig m
     )
  => m ()
f = forever $ do
  updateGameState
  renderGame
  be <- playerSelectBehave
  forM_ be evalBehavior
  enemys <- get @Enemys
  forM_ (Map.toList enemys) $ \(_, Enemy{behave}) -> do
    evalBehavior behave
  updateEnemysBehavior

enemyBehavior
  :: (Has Random sig m, Has (State Enemys) sig m)
  => Index
  -> m Behavior
enemyBehavior index = do
  enemys <- get @Enemys
  let Enemy{damage} = fromJust $ Map.lookup index enemys
  shield <- uniformR (1, 10)
  chooseList
    [ Attack P damage
    , Defend (E index) shield
    ]

updateEnemysBehavior :: (Has Random sig m, Has (State Enemys) sig m) => m ()
updateEnemysBehavior = do
  enemys <- get @Enemys
  ls <- forM (Map.toList enemys) $ \(index, e) -> do
    newBehave <- enemyBehavior index
    pure (index, e{behave = newBehave})
  put (Map.fromList ls)