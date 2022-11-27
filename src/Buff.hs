{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Buff where

import Control.Effect.Labelled (lift)
import Control.Effect.Optics (assign, modifying)
import Control.Effect.Throw (throwError)
import Game.Buff
import Game.Function
import Game.HList
import Game.Input
import Game.Trigger
import Game.Type
import Game.VarMap

absorbDamage :: PBuff
absorbDamage =
  PBuff
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
                        , triggerFun = \SPlayerTakesDamage{enemyAttack} ->
                            Action $ withMaxTriggerTimes 30 $ do
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

resurrectThePlayer :: PBuff
resurrectThePlayer =
  PBuff
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

moreDamage :: PBuff
moreDamage =
  PBuff
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