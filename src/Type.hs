{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Type where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Data.Dynamic (Dynamic, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

data Player = Player
  { health :: Int
  , shield :: Int
  , damage :: Int
  -- , buf :: []
  }
instance Show Player where
  show Player{health, shield, damage} =
    printf "player h: %d s: %d d: %d" health shield damage

data Enemy = Enemy
  { health :: Int
  , shield :: Int
  , damage :: Int
  , behave :: Behavior
  }

instance Show Enemy where
  show Enemy{health, shield, damage, behave} =
    printf
      "enemy h: %d s: %d d: %d next_behave: %s"
      health
      shield
      damage
      (show behave)

type Index = Int

type Enemys = Map Index Enemy

newtype GameState = GameState
  { round :: Int
  }
  deriving (Show)

data Target = P | E Index
  deriving (Show)

data Behavior
  = Attack Target Int
  | SelectEnemyAttack Int
  | RandomSelectEnemyAttack Int
  | Defend Target Int
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
  deriving (Eq, Show, Ord)

newtype RemainingAttack = RemainingAttack Int deriving (Eq, Ord, Show, Typeable)