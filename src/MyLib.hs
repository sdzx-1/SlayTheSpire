{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MyLib where

import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import Control.Monad (forM, forM_, forever)
import Data.Dynamic (fromDynamic)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Effect
import Input
import System.Random (mkStdGen)
import Type
import Utils

runF =
  runLabelledLift
    . runState initGame
    . runRandom (mkStdGen 10)
    . runError @GameError
    $ f

f
  :: forall sig m
   . ( Has Random sig m
     , Has (State Game) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     )
  => m ()
f = forever $ do
  #round %= (+ 1)
  trigger WhenNewTurnStart ()
  renderGame
  be <- playerSelectBehave
  forM_ be evalBehavior
  enemys <- use #enemys
  forM_ (Map.toList enemys) $ \(_, Enemy{behave}) -> do
    evalBehavior behave
  updateEnemysBehavior

enemyBehavior
  :: (Has Random sig m, Has (State Game) sig m)
  => Index
  -> m Behavior
enemyBehavior index = do
  enemys <- use #enemys
  let Enemy{damage} = fromJust $ Map.lookup index enemys
  shield <- uniformR (1, 10)
  chooseList
    [ AttackPlayer damage
    , IncEnemyShield index shield
    ]

updateEnemysBehavior :: (Has Random sig m, Has (State Game) sig m) => m ()
updateEnemysBehavior = do
  enemys <- use #enemys
  ls <- forM (Map.toList enemys) $ \(index, e) -> do
    newBehave <- enemyBehavior index
    pure (index, e{behave = newBehave})
  #enemys .= Map.fromList ls