{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Type where

import Data.Dynamic (Dynamic, Typeable)
import Data.Map (Map)
import GHC.Generics
import Optics (makeFieldLabels)
import Text.Printf (printf)

data Player = Player
  { health :: Int
  , shield :: Int
  , damage :: Int
  }
  deriving (Generic)

instance Show Player where
  show
    Player
      { health
      , shield
      , damage
      } =
      printf
        "player h: %d s: %d d: %d"
        health
        shield
        damage

data Enemy = Enemy
  { health :: Int
  , shield :: Int
  , damage :: Int
  , behave :: Behavior
  }
  deriving (Generic)

instance Show Enemy where
  show
    Enemy
      { health
      , shield
      , damage
      , behave
      } =
      printf
        "enemy h: %d s: %d d: %d next_behave: %s"
        health
        shield
        damage
        (show behave)

type Index = Int

data Behavior
  = AttackEnemy Index Int
  | AttackPlayer Int
  | IncPlayerShield Int
  | IncEnemyShield Index Int
  | SelectEnemyAttack Int
  | RandomSelectEnemyAttack Int
  | IncPlayerHealth Int
  deriving (Show)

data GameError
  = PlayerDeath
  | CleanEnemys
  deriving (Show)

type TriggerMap = Map Trigger [Dynamic -> Maybe Behavior]

data Trigger
  = ThePlayerTakesDamage
  | WhenTheEnemyDies
  | WhenThePlayerSelectDefends
  | WhenThePlayerSelectAttacks
  | WhenNewTurnStart
  deriving (Eq, Show, Ord)

data Game = Game
  { round :: Int
  , player :: Player
  , enemys :: Map Index Enemy
  , triggerMap :: TriggerMap
  }
  deriving (Generic)

instance Show Game where
  show (Game{round, player, enemys}) =
    printf
      "round: %s player: %s enemys: %s"
      (show round)
      (show player)
      (show enemys)

makeFieldLabels ''Player
makeFieldLabels ''Enemy
makeFieldLabels ''Game

newtype RemainingAttack = RemainingAttack Int deriving (Eq, Ord, Show, Typeable)
