{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Type where

import Control.Algebra (Has)
import Control.Effect.Error (Error)
import Control.Effect.Labelled (HasLabelledLift)
import Control.Effect.Random (Random)
import Control.Effect.State (State)
import Data.Dynamic (Dynamic, Typeable)
import Data.IntMap (IntMap)
import Data.Map (Map)
import GHC.Exts (Any, Int (I#), dataToTag#, tagToEnum#)
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
        "🧓  💝: %d s: %d d: %d"
        health
        shield
        damage

data Enemy = Enemy
  { health :: Int
  , shield :: Int
  , damage :: Int
  , behave :: Action
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
        "🦁 h: %d s: %d d: %d next_behave: %s"
        health
        shield
        damage
        (show behave)

type Index = Int

data GameError
  = PlayerDeath
  | CleanEnemys
  | VarError String
  deriving (Show)

type TriggerMap = Map Trigger [Dynamic -> Maybe Action]

data Trigger
  = PlayerTakesDamage
  | PlayerDies
  | TheEnemyDies
  | ThePlayerSelectDefends
  | ThePlayerSelectAttacks
  | NewTurnStart
  deriving (Eq, Show, Ord)

newtype Action = Action
  { runAction
      :: forall m sig
       . ( Has Random sig m
         , Has (Error GameError) sig m
         , HasLabelledLift IO sig m
         , Has (State Game) sig m
         )
      => m ()
  }

instance Show Action where
  show _ = "Action"

type VarMap = Map Int Int

data Game = Game
  { round :: Int
  , player :: Player
  , enemys :: Map Index Enemy
  , triggerMap :: TriggerMap
  , varMap :: Map Int Int
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
