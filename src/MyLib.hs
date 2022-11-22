{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Control.Monad (forM, forM_, forever)
import Data.Dynamic (fromDynamic)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Input
import System.Random (mkStdGen)
import Type
import Utils

runF =
  runLabelledLift
    . runState initGameState
    . runState initPlayer
    . runState initEnemys
    . runState @TriggerMap
      ( Map.fromList
          [
            ( WhenTheEnemyDies
            ,
              [ \dny -> case fromDynamic dny of
                  Nothing -> Nothing
                  Just (RemainingAttack i) -> Just $ RandomSelectEnemyAttack i
              ]
            )
          ]
      )
    . runRandom (mkStdGen 10)
    . runError @GameError
    $ f

f
  :: forall sig m
   . ( Has Random sig m
     , Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (State GameState) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State TriggerMap) sig m
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