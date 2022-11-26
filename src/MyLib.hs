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
import Control.Monad (forM_, forever, join, when)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Random (mkStdGen)

import Control.Carrier.Fresh.Strict (runFresh)
import Control.Carrier.Reader (ask)
import Control.Effect.Optics (assign, modifying, use)
import qualified Data.IntMap as IntMap
import Game.Buff
import Game.Function
import Game.Trigger
import Game.Type
import Game.VarMap

initEnemys =
  IntMap.fromList
    [ (i, Enemy 10 10 10)
    | i <- [0 .. 10]
    ]

runF =
  runLabelledLift
    . runState (Player 100 0 1)
    . runState (Game 0 initEnemys)
    . runState (VarMap Map.empty)
    . runState (BuffMap Map.empty)
    . runState (TriggerMap IntMap.empty)
    . runFresh 0
    . runRandom (mkStdGen 10)
    . runError @GameError
    $ f
      [ PBuff
          { buffName = BuffName "The player can revive 10 times after death"
          , buffInit = BuffInit @'[PlayerDies] $ do
              times <- definedVar 0
              pure
                ( ( 0
                  , \SPlayerDies -> Action $ do
                      modifyVar times (+ 1)
                      tv <- useVar times
                      when (tv >= 10) $ do
                        lift $ putStrLn "------- REMOVE BUFF SELF -------"
                        buffIndex <- ask @BuffIndex
                        cleanBuff buffIndex
                        throwError BuffEarlyExist
                      lift $ putStrLn "player dies, revive, set health 100"
                      assign @Player #health 100
                  )
                    ::: HNil
                , [times]
                )
          }
      , PBuff
          { buffName = BuffName "remain attack to select new enemy killed"
          , buffInit = BuffInit @'[EnemyDies, NewTurnStart] $ do
              times <- definedVar 0
              turnV <- definedVar 0
              pure
                ( ( 0
                  , \SEnemyDies{remainAttack} -> Action $ do
                      modifyVar times (+ 1)
                      val <- useVar turnV
                      tv <- useVar times
                      when (tv > 5) $ do
                        lift $ putStrLn "------- REMOVE BUFF SELF -------"
                        buffIndex <- ask @BuffIndex
                        cleanBuff buffIndex
                        throwError BuffEarlyExist
                      lift $
                        putStrLn $
                          "buff: remainAttack "
                            ++ show remainAttack
                            ++ " + trun add "
                            ++ show val
                            ++ ", times: "
                            ++ show tv
                      randomSelectEnemyAttack remainAttack
                  )
                    ::: ( 0
                        , \SNewTurnStart -> Action $ do
                            modifyVar turnV (+ 10)
                            val <- useVar turnV
                            lift $ putStrLn $ "new turn inc damage: " ++ show val
                        )
                    ::: HNil
                , [times, turnV]
                )
          }
      ]

f
  :: forall sig m
   . All sig m
  => [PBuff]
  -> m ()
f pbs = do
  forM_ pbs $ \pb@PBuff{buffName, buffInit} -> do
    lift $ putStrLn $ "init buff: " ++ show pb
    initBuff buffName buffInit
  forever $ do
    modifying @_ @Game #round (+ 1)
    trigger SNewTurnStart
    renderGame
    be <- playerSelectBehave
    case be of
      Nothing -> pure ()
      Just f' -> f'
    enemys <- use @Game #enemys
    forM_ (IntMap.keys enemys) $ \index -> do
      join $ enemyBehavior index

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
