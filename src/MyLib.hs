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

import Control.Carrier.Fresh.Strict (runFresh)
import Control.Effect.Optics (assign, modifying, use)
import qualified Data.IntMap as IntMap
import Game.Buff
import Game.Function
import Game.HList
import Game.Input
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
          { buffName = BuffName "Converts enemy attacks to damage 30 times"
          , buffDefined =
              definedBuff @'[PlayerTakesDamage]
                BuffDesc
                  { defaultVarValues = 0 :* 0 :* VNil
                  , triggerFunsDefined = \(times :* totalDamageInc :* _) ->
                      BuffBehave
                        { triggerFunList =
                            PriorityAndTriggerFun
                              { priority = 0
                              , triggerFun = \SPlayerTakesDamage{enemyAttack} -> Action $ withMaxTriggerTimes 30 $ do
                                  modifyVar times (+ 1)
                                  modifyVar totalDamageInc (+ enemyAttack)
                                  lift $ putStrLn "Converts enemy attacks to damage, interrupt enemy attack"
                                  modifying @Player #damage (+ enemyAttack)
                                  throwError InterruptAttack
                              }
                              ::: HNil
                        , description = Desciption $ do
                            times' <- useVar times
                            tdi' <- useVar totalDamageInc
                            pure $
                              "now times: [" ++ show times' ++ "], " ++ "total damage inc: [" ++ show tdi' ++ "]"
                        }
                  }
          }
      , PBuff
          { buffName = BuffName "The player can revive 10 times after death"
          , buffDefined =
              definedBuff @'[PlayerDies]
                BuffDesc
                  { defaultVarValues = 0 :* VNil
                  , triggerFunsDefined = \(times :* _) ->
                      BuffBehave
                        { triggerFunList =
                            PriorityAndTriggerFun
                              { priority = 0
                              , triggerFun = \SPlayerDies -> Action $ withMaxTriggerTimes 10 $ do
                                  modifyVar times (+ 1)
                                  lift $ putStrLn "player dies, revive, select health"
                                  let healthList = Avi "SELECT HEALTH" [(a, b, show b) | a <- [1 .. 7], let b = a * 10]
                                  res' <- getInput (healthList :- N)
                                  case res' of
                                    Nothing -> assign @Player #health 100
                                    Just (I h :- N) -> assign @Player #health h
                              }
                              ::: HNil
                        , description = Desciption $ do
                            times' <- useVar times
                            pure $ "player dies times: [" ++ show times' ++ "]"
                        }
                  }
          }
      , PBuff
          { buffName = BuffName "remain attack to select new enemy killed"
          , buffDefined =
              definedBuff @'[EnemyDies, NewTurnStart]
                BuffDesc
                  { defaultVarValues = 0 :* VNil
                  , triggerFunsDefined = \(turnV :* _) ->
                      BuffBehave
                        { triggerFunList =
                            PriorityAndTriggerFun
                              { priority = 0
                              , triggerFun = \SEnemyDies{remainAttack} -> Action $ do
                                  val <- useVar turnV
                                  lift $ putStrLn $ "buff: remainAttack " ++ show remainAttack ++ " + trun add " ++ show val
                                  randomSelectEnemyAttack (remainAttack + val)
                              }
                              ::: PriorityAndTriggerFun
                                { priority = 0
                                , triggerFun = \SNewTurnStart -> Action $ do
                                    modifyVar turnV (+ 1)
                                    val <- useVar turnV
                                    lift $ putStrLn $ "new turn inc damage: " ++ show val
                                }
                              ::: HNil
                        , description = Desciption $ do
                            v <- useVar turnV
                            pure $ "add damage [" ++ show v ++ "]"
                        }
                  }
          }
      ]
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
