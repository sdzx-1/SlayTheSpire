{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import Control.Monad (forM_, forever, void, when)
import Data.Dynamic
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Input
import Text.Read (readMaybe)
import Type

initGameState :: GameState
initGameState =
  GameState
    { round = 0
    }

initPlayer :: Player
initPlayer = Player 100 0 1

initEnemys :: Map Index Enemy
initEnemys =
  Map.fromList
    [ (0, Enemy 20 0 5 $ Attack P 5)
    , (1, Enemy 20 0 2 $ Attack P 2)
    , (2, Enemy 20 0 1 $ Attack P 1)
    , (3, Enemy 20 0 3 $ Attack P 3)
    ]
chooseList :: Has Random sig m => [a] -> m a
chooseList ls = do
  let len = length ls
  i <- uniformR (0, len - 1)
  pure $ ls !! i

getEnemyTarget :: (Has Random sig m, Has (State Enemys) sig m) => m Index
getEnemyTarget = do
  enemys <- get @Enemys
  let es = Map.keys enemys
  chooseList es

updateGameState :: (Has (State GameState) sig m) => m ()
updateGameState = do
  g@GameState{round} <- get
  put (g{round = round + 1})

damagePlayer
  :: ( Has (Error GameError) sig m
     , Has (State Player) sig m
     )
  => Int
  -> m ()
damagePlayer i = do
  p@Player{health, shield} <- get
  if i > shield
    then do
      let newHealth = health - (i - shield)
      when (newHealth <= 0) $ throwError PlayerDeath
      put @Player p{health = newHealth, shield = 0}
    else put @Player p{shield = shield - i}

damageEnemy
  :: ( Has (State Enemys) sig m
     , Has (State Player) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State TriggerMap) sig m
     , Has Random sig m
     )
  => Index
  -> Int
  -> m ()
damageEnemy index i = do
  enemys <- get
  case Map.lookup index enemys of
    Nothing -> pure ()
    Just e@Enemy{health, shield} -> do
      if i > shield
        then do
          let newHealth = health - (i - shield)
          if newHealth <= 0
            then do
              lift $ putStrLn $ "+ KILL ENEMY: " ++ show index
              modify @Enemys (Map.delete index)
              enemys' <- get @Enemys
              when (Map.null enemys') (throwError CleanEnemys)
              trigger WhenTheEnemyDies (RemainingAttack (-newHealth))
            else modify (Map.insert index (e{health = newHealth, shield = 0} :: Enemy))
        else modify (Map.insert index (e{shield = shield - i} :: Enemy))

defendPlayer :: (Has (State Player)) sig m => Int -> m ()
defendPlayer i = do
  p@Player{shield} <- get
  put @Player (p{shield = shield + i})

defendEnemy
  :: Has (State Enemys) sig m
  => Index
  -> Int
  -> m ()
defendEnemy index i = do
  enemys <- get @Enemys
  case Map.lookup index enemys of
    Nothing -> pure ()
    Just e@Enemy{shield} ->
      modify (Map.insert index (e{shield = shield + i} :: Enemy))

renderGame
  :: ( Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (State GameState) sig m
     , HasLabelledLift IO sig m
     )
  => m ()
renderGame = do
  player <- get @Player
  enemys <- Map.toList <$> get @Enemys
  GameState{round} <- get
  let game = "round " ++ show round
      strls = map (\(i, e) -> show i ++ ", " ++ show e) enemys
  lift $ putStrLn $ unlines (game : show player : strls)

playerSelectBehave
  :: forall sig m
   . ( Has (State Enemys) sig m
     , Has (State Player) sig m
     , HasLabelledLift IO sig m
     )
  => m (Maybe Behavior)
playerSelectBehave = do
  res <-
    getInput
      ( Avi
          "SELECT BEHAVE"
          [ (1, 1 :: Int, "attack")
          , (2, 2, "defend")
          , (3, 3, "player_info")
          , (4, 4, "enemys_info")
          ]
          :+ ANil
      )
  case res of
    Nothing -> pure Nothing
    Just (r :- RNil) -> case r of
      1 -> do
        enemys <- Map.toList <$> get @Enemys
        let ts = Avi "SELECT Enemy" $ zipWith (curry (\(a, (b, c)) -> (a, b, show c))) [1 ..] enemys
            baseDamage = Avi "SELECT Basedamage" [(a, b :: Int, show b) | a <- [1 .. 9], let b = a * 10]
        res' <- getInput (ts :+ baseDamage :+ ANil)
        case res' of
          Nothing -> playerSelectBehave
          Just (target :- baseD :- RNil) -> do
            Player{damage} <- get
            pure (Just $ Attack (E target) (baseD + damage))
      2 -> do
        let baseShield = Avi "SELECT Shield" [(a, b :: Int, show b) | a <- [1 .. 9], let b = a * 10]
        res' <- getInput (baseShield :+ ANil)
        case res' of
          Nothing -> playerSelectBehave
          Just (baseS :- RNil) -> do
            pure (Just $ Defend P baseS)
      3 -> do
        p <- get @Player
        lift $ print p
        playerSelectBehave
      4 -> do
        enemys <- get @Enemys
        lift $ putStrLn (unlines $ map show (Map.toList enemys))
        playerSelectBehave
      _ -> pure Nothing

trigger
  :: ( Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (Error GameError) sig m
     , Has Random sig m
     , Has (State TriggerMap) sig m
     , HasLabelledLift IO sig m
     , Typeable a
     )
  => Trigger
  -> a
  -> m ()
trigger t a = do
  tm <- get @TriggerMap
  case Map.lookup t tm of
    Nothing -> pure ()
    Just xs -> do
      let dny = toDyn a
      forM_ xs $ \x -> forM_ (x dny) evalBehavior

evalBehavior
  :: ( Has (State Player) sig m
     , Has (State Enemys) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State TriggerMap) sig m
     , Has Random sig m
     )
  => Behavior
  -> m ()
evalBehavior b = do
  lift $ print b
  case b of
    Attack t v -> case t of
      P -> damagePlayer v
      E index -> damageEnemy index v
    SelectEnemyAttack i -> do
      enemys <- Map.toList <$> get @Enemys
      let ts = Avi "SELECT Enemy" $ zipWith (curry (\(a, (b', c)) -> (a, b', show c))) [1 ..] enemys
      res' <- getInput (ts :+ ANil)
      case res' of
        Nothing -> pure ()
        Just (target :- RNil) -> do
          damageEnemy target i
    RandomSelectEnemyAttack i -> do
      enemys <- Map.keys <$> get @Enemys
      co <- chooseList enemys
      damageEnemy co i
    Defend t v -> case t of
      P -> defendPlayer v
      E index -> defendEnemy index v
    IncPlayerHealth i -> do
      p@Player{health} <- get
      put @Player (p{health = health + i})
