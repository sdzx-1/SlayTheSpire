{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Type where

import Data.IntMap (IntMap)
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
        "🧑 ❤️ %d 🛡 %d  🗡️ %d"
        health
        shield
        damage

data Enemy = Enemy
  { health :: Int
  , shield :: Int
  , damage :: Int
  }
  deriving (Generic)

instance Show Enemy where
  show
    Enemy
      { health
      , shield
      , damage
      } =
      printf
        "👺 ❤ %d 🛡 %d  ️🗡 %d "
        health
        shield
        damage

type Index = Int

data GameError
  = PlayerDeath
  | CleanEnemys
  | VarError String
  deriving (Show)

type Enemys = IntMap Enemy

data Game = Game {round :: Int, enemys :: Enemys}
  deriving (Show, Generic)

makeFieldLabels ''Player
makeFieldLabels ''Enemy
makeFieldLabels ''Game