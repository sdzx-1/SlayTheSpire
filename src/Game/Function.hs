{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game.Function where

import Control.Algebra (Has)
import Control.Effect.Error (throwError)
import Control.Effect.Labelled (lift)
import Control.Effect.Optics (assign, modifying, use, uses)
import Control.Effect.Random (Random, uniformR)
import Control.Effect.State (get)
import Control.Monad (when)
import qualified Data.IntMap as IntMap
import Game.Buff
import Game.Input (AvailableList (ANil, (:+)), Avi (Avi), ResultList (RNil, (:-)), getInput)
import Game.Trigger
import Game.Type
import Optics (At (..), (%?))
import Optics.Optic ((%))

chooseList :: Has Random sig m => [a] -> m a
chooseList ls = do
  let len = length ls
  i <- uniformR (0, len - 1)
  pure $ ls !! i

incPlayerShield i = modifying @_ @Player #shield (+ i)

incEnemyShield index i = modifying @_ @Game (#enemys % at index %? #shield) (+ i)

incPlayerHealth h = modifying @_ @Player #health (+ h)

randomSelectEnemyAttack i = do
  es <- uses @Game #enemys IntMap.keys
  co <- chooseList es
  damageEnemy co i

selectEnemyAttack i = do
  enemys <- IntMap.toList <$> use @Game #enemys
  let ts = Avi "SELECT Enemy" $ zipWith (curry (\(a, (b', c)) -> (a, b', show c))) [1 ..] enemys
  res' <- getInput (ts :+ ANil)
  case res' of
    Nothing -> pure ()
    Just (target :- RNil) ->
      damageEnemy target i

damagePlayer
  :: ( All sig m
     )
  => Int
  -> m ()
damagePlayer i = do
  lift $ putStrLn $ "🏹🧑: " ++ show i
  shield <- use @Player #shield
  if i > shield
    then do
      health <- use @Player #health
      let newHealth = health - (i - shield)
      when (newHealth <= 0) $ trigger SPlayerDies
      newHealth' <- use @Player #health
      when (newHealth' <= 0) $ throwError PlayerDeath
      assign @Player #shield 0
      assign @Player #health newHealth
    else assign @Player #shield (shield - i)

damageEnemy
  :: ( All sig m
     )
  => Index
  -> Int
  -> m ()
damageEnemy index i = do
  lift $ putStrLn $ "🏹👺: " ++ show index ++ ", " ++ show i
  s <- use @Game $ #enemys % at index
  case s of
    Nothing -> pure ()
    Just Enemy{health, shield} -> do
      if i > shield
        then do
          let newHealth = health + shield - i
          if newHealth <= 0
            then do
              lift $ putStrLn $ "+ KILL ENEMY: " ++ show index
              assign @Game (#enemys % at index) Nothing
              isNull <- uses @Game #enemys IntMap.null
              when isNull $ throwError CleanEnemys
              trigger (SEnemyDies (-newHealth) index)
            else do
              assign @Game (#enemys % at index %? #shield) 0
              assign @Game (#enemys % at index %? #health) newHealth
        else assign @Game (#enemys % at index %? #shield) (shield - i)

renderGame
  :: All sig m
  => m ()
renderGame = do
  player <- get @Player
  enemys <- IntMap.toList <$> use @Game #enemys
  round' <- use @Game #round
  let game = "round " ++ show round'
      strls = map (\(i, e) -> show i ++ ", " ++ show e) enemys
  lift $ putStrLn $ unlines (game : show player : strls)

playerSelectBehave
  :: forall sig m
   . All sig m
  => m (Maybe (m ()))
playerSelectBehave = do
  res <-
    getInput
      ( Avi
          "SELECT BEHAVE"
          [ (1, 1 :: Int, "⚔️")
          , (2, 2, "🛡")
          , (3, 3, "player_info")
          , (4, 4, "enemys_info")
          ]
          :+ ANil
      )
  case res of
    Nothing -> pure Nothing
    Just (r :- RNil) -> case r of
      1 -> do
        enemys <- IntMap.toList <$> use @Game #enemys
        let ts = Avi "SELECT Enemy" $ zipWith (curry (\(a, (b, c)) -> (a, b, show c))) [1 ..] enemys
            baseDamage = Avi "SELECT Basedamage" [(a, b :: Int, show b) | a <- [1 .. 9], let b = a * 10]
        res' <- getInput (ts :+ baseDamage :+ ANil)
        case res' of
          Nothing -> playerSelectBehave
          Just (target :- baseD :- RNil) -> do
            damage <- use @Player #damage
            pure (Just $ damageEnemy target (baseD + damage))
      2 -> do
        let baseShield = Avi "SELECT Shield" [(a, b :: Int, show b) | a <- [1 .. 9], let b = a * 10]
        res' <- getInput (baseShield :+ ANil)
        case res' of
          Nothing -> playerSelectBehave
          Just (baseS :- RNil) -> do
            pure (Just $ modifying @_ @Player #shield (+ baseS))
      3 -> do
        p <- get @Player
        lift $ print p
        playerSelectBehave
      4 -> do
        enemys <- use @Game #enemys
        lift $ putStrLn (unlines $ map show (IntMap.toList enemys))
        playerSelectBehave
      _ -> pure Nothing