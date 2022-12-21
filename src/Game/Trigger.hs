{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Game.Trigger where

import Data.Data (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)

data Trigger
  = PlayerTakesDamage
  | PlayerDies
  | EnemyDies
  | PlayerSelectDefends
  | PlayerSelectAttacks
  | NewTurnStart
  | TurnEnd
  deriving (Eq, Show, Ord)

type family ToIndex (a :: Trigger) :: Nat where
  ToIndex PlayerTakesDamage = 1
  ToIndex PlayerDies = 2
  ToIndex EnemyDies = 3
  ToIndex PlayerSelectDefends = 4
  ToIndex PlayerSelectAttacks = 5
  ToIndex NewTurnStart = 6
  ToIndex TurnEnd = 7

data STrigger (p :: Trigger) where
  SPlayerTakesDamage :: {enemyAttack :: Int} -> STrigger PlayerTakesDamage
  SPlayerDies :: STrigger PlayerDies
  SEnemyDies :: {remainAttack :: Int, enemyId :: Int} -> STrigger EnemyDies
  SPlayerSelectDefends :: STrigger PlayerSelectDefends
  SPlayerSelectAttacks :: STrigger PlayerSelectAttacks
  SNewTurnStart :: STrigger NewTurnStart
  STurnEnd :: STrigger TurnEnd

toIndex :: forall (p :: Trigger). (KnownNat (ToIndex p)) => Int
toIndex = fromIntegral (natVal @(ToIndex p) Proxy)

ix :: forall p. (KnownNat (ToIndex p)) => STrigger p -> Int
ix _ = toIndex @p

ixF :: forall p a. (KnownNat (ToIndex p)) => (STrigger p -> a) -> Int
ixF _ = toIndex @p