{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Utils where

import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen
import Control.Carrier.State.Strict
import Control.Effect.Fresh (Fresh, fresh)
import Control.Effect.Labelled
import Control.Monad (forM_, when)
import Data.Data (Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map)
import qualified Data.Map as Map
import Effect
import Input
import Optics (At (..), (%?))
import Optics.Optic ((%))
import Type

initGame :: Game
initGame =
  Game
    { round = 0
    , player = initPlayer
    , enemys = initEnemys
    , varMap = Map.fromList [(1, 0)]
    , triggerMap =
        Map.fromList
          [
            ( TheEnemyDies
            ,
              [ \_ -> Just $ Action $ uniformR (10, 300) >>= damagePlayer
              , \dny -> case fromDynamic dny of
                  Nothing -> Nothing
                  Just (RemainingAttack i) ->
                    Just $ Action $ selectEnemyAttack i
              ]
            )
          ,
            ( PlayerDies
            ,
              [ \_ -> Just $ Action $ do
                  modifyVar 1 (+ 1)
                  i <- getVar 1
                  if i >= 2
                    then pure ()
                    else do
                      lift $ putStrLn $ "player dies " ++ show i ++ ", set health to 100"
                      #player % #health .= 100
              ]
            )
          ,
            ( NewTurnStart
            ,
              [ \dny -> case fromDynamic dny of
                  Nothing -> Nothing
                  Just () -> Just $ Action $ #player % #health %= (+ 10)
              ]
            )
          ]
    }

initPlayer :: Player
initPlayer = Player 100 0 1

initEnemys :: Map Index Enemy
initEnemys =
  Map.fromList
    [ (0, Enemy 20 0 5 $ Action $ damagePlayer 5)
    , (1, Enemy 20 0 2 $ Action $ damagePlayer 2)
    , (2, Enemy 20 0 1 $ Action $ damagePlayer 1)
    , (3, Enemy 20 0 3 $ Action $ damagePlayer 3)
    ]
chooseList :: Has Random sig m => [a] -> m a
chooseList ls = do
  let len = length ls
  i <- uniformR (0, len - 1)
  pure $ ls !! i

initAction
  :: ( Has (State Game) sig m
     , Has Fresh sig m
     )
  => m (Int, Trigger, Int, Dynamic -> Maybe Action)
initAction = do
  fi <- newVar
  pure
    ( 1
    , PlayerDies
    , 10
    , \_ -> Just $ Action $ do
        modifyVar fi (+ 1)
        i <- getVar fi
        if i >= 2
          then pure ()
          else do
            lift $
              putStrLn $
                "player dies "
                  ++ show i
                  ++ ", set health to 100"
            #player % #health .= 100
    )

newVar
  :: ( Has (State Game) sig m
     , Has Fresh sig m
     )
  => m Int
newVar = do
  i <- fresh
  #varMap % at i .= Just 0
  pure i

getVar
  :: ( Has (State Game) sig m
     , Has (Error GameError) sig m
     )
  => Int
  -> m Int
getVar i = do
  use (#varMap % at i) >>= \case
    Nothing -> throwError (VarError $ show i ++ " undefined")
    Just v -> pure v

setVar
  :: ( Has (State Game) sig m
     , Has (Error GameError) sig m
     )
  => Int
  -> Int
  -> m ()
setVar i v = do
  use (#varMap % at i) >>= \case
    Nothing -> throwError (VarError $ show i ++ " undefined")
    Just _ -> #varMap % at i .= Just v

modifyVar
  :: ( Has (State Game) sig m
     , Has (Error GameError) sig m
     )
  => Int
  -> (Int -> Int)
  -> m ()
modifyVar i f = do
  use (#varMap % at i) >>= \case
    Nothing -> throwError (VarError $ show i ++ " undefined")
    Just v -> #varMap % at i .= Just (f v)

getEnemyTarget
  :: ( Has Random sig m
     , Has (State Game) sig m
     )
  => m Index
getEnemyTarget = uses #enemys Map.keys >>= chooseList

damagePlayer
  :: ( Has (Error GameError) sig m
     , Has (State Game) sig m
     , HasLabelledLift IO sig m
     , Has Random sig m
     )
  => Int
  -> m ()
damagePlayer i = do
  lift $ putStrLn $ "attack PLAYER: " ++ show i
  shield <- use (#player % #shield)
  if i > shield
    then do
      health <- use (#player % #health)
      let newHealth = health - (i - shield)
      when (newHealth <= 0) $ trigger PlayerDies ()
      newHealth' <- use $ #player % #health
      when (newHealth' <= 0) $ throwError PlayerDeath
      #player % #shield .= 0
      #player % #health .= newHealth
    else #player % #shield .= (shield - i)

damageEnemy
  :: ( Has (State Game) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has Random sig m
     )
  => Index
  -> Int
  -> m ()
damageEnemy index i = do
  lift $ putStrLn $ "attack ENEMY: " ++ show index ++ ", " ++ show i
  s <- use $ #enemys % at index
  case s of
    Nothing -> pure ()
    Just Enemy{health, shield} -> do
      if i > shield
        then do
          let newHealth = health + shield - i
          if newHealth <= 0
            then do
              lift $ putStrLn $ "+ KILL ENEMY: " ++ show index
              #enemys % at index .= Nothing
              isNull <- uses #enemys Map.null
              when isNull $ throwError CleanEnemys
              trigger TheEnemyDies (RemainingAttack (-newHealth))
            else do
              #enemys % at index %? #shield .= 0
              #enemys % at index %? #health .= newHealth
        else #enemys % at index %? #shield .= shield - i

renderGame
  :: ( Has (State Game) sig m
     , HasLabelledLift IO sig m
     )
  => m ()
renderGame = do
  player <- use #player
  enemys <- Map.toList <$> use #enemys
  round' <- use #round
  let game = "round " ++ show round'
      strls = map (\(i, e) -> show i ++ ", " ++ show e) enemys
  lift $ putStrLn $ unlines (game : show player : strls)

playerSelectBehave
  :: forall sig m
   . ( Has (State Game) sig m
     , HasLabelledLift IO sig m
     )
  => m (Maybe Action)
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
        enemys <- Map.toList <$> use #enemys
        let ts = Avi "SELECT Enemy" $ zipWith (curry (\(a, (b, c)) -> (a, b, show c))) [1 ..] enemys
            baseDamage = Avi "SELECT Basedamage" [(a, b :: Int, show b) | a <- [1 .. 9], let b = a * 10]
        res' <- getInput (ts :+ baseDamage :+ ANil)
        case res' of
          Nothing -> playerSelectBehave
          Just (target :- baseD :- RNil) -> do
            damage <- use $ #player % #damage
            pure (Just $ Action $ damageEnemy target (baseD + damage))
      2 -> do
        let baseShield = Avi "SELECT Shield" [(a, b :: Int, show b) | a <- [1 .. 9], let b = a * 10]
        res' <- getInput (baseShield :+ ANil)
        case res' of
          Nothing -> playerSelectBehave
          Just (baseS :- RNil) -> do
            pure (Just $ Action $ #player % #shield %= (+ baseS))
      3 -> do
        p <- use #player
        lift $ print p
        playerSelectBehave
      4 -> do
        enemys <- use #enemys
        lift $ putStrLn (unlines $ map show (Map.toList enemys))
        playerSelectBehave
      _ -> pure Nothing

trigger
  :: ( Has (State Game) sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has Random sig m
     , Typeable a
     )
  => Trigger
  -> a
  -> m ()
trigger t a = do
  tm <- use $ #triggerMap % at t
  case tm of
    Nothing -> pure ()
    Just xs -> do
      let dny = toDyn a
      forM_ xs $ \x -> case x dny of
        Nothing -> pure ()
        Just (Action f) -> f

incPlayerShield i = #player % #shield %= (+ i)
incEnemyShield index i = #enemys % at index %? #shield %= (+ i)
incPlayerHealth h = #player % #health %= (+ h)
randomSelectEnemyAttack i = do
  es <- uses #enemys Map.keys
  co <- chooseList es
  damageEnemy co i
selectEnemyAttack i = do
  enemys <- Map.toList <$> use #enemys
  let ts = Avi "SELECT Enemy" $ zipWith (curry (\(a, (b', c)) -> (a, b', show c))) [1 ..] enemys
  res' <- getInput (ts :+ ANil)
  case res' of
    Nothing -> pure ()
    Just (target :- RNil) -> damageEnemy target i
