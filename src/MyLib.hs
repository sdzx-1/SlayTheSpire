{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MyLib where

import Control.Carrier.Random.Gen
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector hiding ((++))
import Data.Vector.Mutable
import System.Random (mkStdGen)
import Text.Printf (printf)

data Player = Player
  { health :: Int
  , shield :: Int
  , damage :: Int
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
  show Enemy{health, shield, damage} =
    printf "enemy h: %d s: %d d: %d" health shield damage

type Index = Int

type Enemys = Map Index Enemy

newtype GameState = GameState
  { round :: Int
  }
  deriving (Show)

data Target = P | E Index
  deriving (Show)

data Behavior
  = Attack Target
  | Defend
  deriving (Show)

initGameState :: GameState
initGameState =
  GameState
    { round = 0
    }

initPlayer :: Player
initPlayer = Player 100 100 1

initEnemys :: Map Index Enemy
initEnemys =
  Map.fromList
    [ (0, Enemy 100 0 1 $ Attack P)
    , (1, Enemy 100 0 1 $ Attack P)
    , (2, Enemy 100 0 1 $ Attack P)
    , (3, Enemy 100 0 1 $ Attack P)
    ]

runF =
  runLabelledLift
    . runState initGameState
    . runState initPlayer
    . runState initEnemys
    . runRandom (mkStdGen 10)
    $ f

f
  :: forall sig m
   . ( Has Random sig m
     , Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (State GameState) sig m
     , HasLabelledLift IO sig m
     )
  => m ()
f = do
  st@GameState{round} <- get @GameState
  lift $ print st