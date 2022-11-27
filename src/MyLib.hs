{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module MyLib where

import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import Control.Monad (forM_, forever, join)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Random (mkStdGen)

import Buff
import Control.Carrier.Fresh.Strict (runFresh)
import Control.Effect.Optics (modifying, use)
import qualified Data.IntMap as IntMap
import Game.Buff
import Game.Function
import Game.Trigger
import Game.Type
import Game.VarMap

initEnemys :: IntMap.IntMap Enemy
initEnemys =
  IntMap.fromList
    [ (i, Enemy 10 10 10)
    | i <- [0 .. 10]
    ]

runF :: IO ()
runF = do
  res <-
    runLabelledLift
      . runState (Player 100 0 1)
      . runState (Game 0 initEnemys)
      . runState (VarMap Map.empty)
      . runState (BuffMap Map.empty)
      . runState (TriggerMap IntMap.empty)
      . runFresh 0
      . runRandom (mkStdGen 10)
      . runError @GameError
      $ f [absorbDamage, resurrectThePlayer, moreDamage, temporary_increase_shield 100]
  print res
f
  :: forall sig m
   . All sig m
  => [PBuff]
  -> m ()
f pbs = do
  forM_ pbs $ \pb@PBuff{buffName, buffDefined} -> do
    lift $ putStrLn $ "init buff: " ++ show pb
    initBuff buffName buffDefined
  forever $ do
    modifying @Game #round (+ 1)
    trigger SNewTurnStart
    renderGame
    be <- playerSelectBehave
    case be of
      Nothing -> pure ()
      Just f' -> f'
    enemys <- use @Game #enemys
    forM_ (IntMap.keys enemys) $ \index -> do
      join $ enemyBehavior index
    trigger STurnEnd

enemyBehavior
  :: All sig m
  => Index
  -> m (m ())
enemyBehavior index = do
  enemys <- use @Game #enemys
  let Enemy{damage} = fromJust $ IntMap.lookup index enemys
  shield <- uniformR (1, 10)
  chooseList
    [ damagePlayer damage
    , incEnemyShield index shield
    ]
