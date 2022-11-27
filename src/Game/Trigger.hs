{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Game.Trigger where

data Trigger
  = PlayerTakesDamage
  | PlayerDies
  | EnemyDies
  | PlayerSelectDefends
  | PlayerSelectAttacks
  | NewTurnStart
  | TurnEnd
  deriving (Eq, Show, Ord)

data STrigger (p :: Trigger) where
  SPlayerTakesDamage :: {enemyAttack :: Int} -> STrigger PlayerTakesDamage
  SPlayerDies :: STrigger PlayerDies
  SEnemyDies :: {remainAttack :: Int, enemyId :: Int} -> STrigger EnemyDies
  SPlayerSelectDefends :: STrigger PlayerSelectDefends
  SPlayerSelectAttacks :: STrigger PlayerSelectAttacks
  SNewTurnStart :: STrigger NewTurnStart
  STurnEnd :: STrigger TurnEnd

class IX (p :: Trigger) where
  ix :: STrigger p -> Int
  ixF :: (STrigger p -> a) -> Int

ixT :: Trigger -> Int
ixT PlayerTakesDamage = 1
ixT PlayerDies = 2
ixT EnemyDies = 3
ixT PlayerSelectDefends = 4
ixT PlayerSelectAttacks = 5
ixT NewTurnStart = 6
ixT TurnEnd = 7

instance IX 'PlayerTakesDamage where
  ix _ = 1
  ixF _ = 1
instance IX 'PlayerDies where
  ix _ = 2
  ixF _ = 2
instance IX 'EnemyDies where
  ix _ = 3
  ixF _ = 3
instance IX 'PlayerSelectDefends where
  ix _ = 4
  ixF _ = 4
instance IX 'PlayerSelectAttacks where
  ix _ = 5
  ixF _ = 5
instance IX 'NewTurnStart where
  ix _ = 6
  ixF _ = 6

instance IX 'TurnEnd where
  ix _ = 7
  ixF _ = 7