{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Game.Buff where

import Control.Algebra (Has)
import Control.Carrier.Reader (runReader)
import Control.Effect.Error (Error, catchError, throwError)
import Control.Effect.Fresh (Fresh, fresh)
import Control.Effect.Labelled (HasLabelledLift)
import Control.Effect.Random (Random)
import Control.Effect.Reader (Reader)
import Control.Effect.State (State, get, modify)
import Control.Monad (forM_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Exts (Any)
import Game.Trigger
import Game.Type
import Game.VarMap (VarMap, VarRef, definedVar, deleteVar, modifyVar)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

data PBuff = PBuff
  { buffName :: BuffName
  , buffInit :: BuffInit
  }
  deriving (Show)

type TriggerFun x = STrigger x -> Action

data HList (xs :: [Trigger]) where
  HNil :: HList '[]
  (:::) :: (Int, TriggerFun x) -> HList xs -> HList (x ': xs)

infixr 5 :::

class ToIXs (xs :: [Trigger]) where
  toIxs :: HList xs -> [Int]

instance ToIXs '[] where
  toIxs HNil = []

instance (IX x, ToIXs xs) => ToIXs (x ': xs) where
  toIxs (a ::: b) = ixF (snd a) : toIxs b

class InsertTriggerMap xs where
  itmap
    :: ( Has (State TriggerMap) sig m
       , Has (State VarMap) sig m
       , Has Fresh sig m
       )
    => BuffIndex
    -> HList xs
    -> m ()

instance InsertTriggerMap '[] where
  itmap _ HNil = pure ()

instance (IX x, InsertTriggerMap xs) => InsertTriggerMap (x ': xs) where
  itmap buffIndex ((priority, triggerFun) ::: b) = do
    timesVarRef <- definedVar 0
    modify @TriggerMap (insertTrigger (TriggerInfo{buffIndex, priority, triggerFun, timesVarRef}))
    itmap buffIndex b

data BuffInit = forall xs.
  (InsertTriggerMap xs, ToIXs xs) =>
  BuffInit {buffInit :: forall sig m. (All sig m) => m (HList xs, [VarRef])}

instance Show BuffInit where
  show _ = " BuffInit "

data BuffRef = BuffRef
  { buffVarRef :: [VarRef]
  , buffTriggerRef :: [Int]
  }
  deriving (Show)

cleanBuff
  :: ( Has (State BuffMap) sig m
     , Has (State VarMap) sig m
     , Has (State TriggerMap) sig m
     )
  => BuffIndex
  -> m ()
cleanBuff buffIndex = do
  buffMap' <- buffMap <$> get @BuffMap
  case Map.lookup buffIndex buffMap' of
    Nothing -> pure ()
    Just Buff{buffRef = BuffRef{buffVarRef, buffTriggerRef}} -> do
      mapM_ deleteVar buffVarRef
      mapM_ (deleteTrigger buffIndex) buffTriggerRef
      modify @BuffMap (BuffMap . Map.delete buffIndex . buffMap)

initBuff :: All sig m => BuffName -> BuffInit -> m BuffIndex
initBuff buffName BuffInit{buffInit} = do
  buffIndex <- BuffIndex <$> fresh
  (fun, varRefs) <- buffInit
  itmap buffIndex fun
  let buffRef = BuffRef{buffVarRef = varRefs, buffTriggerRef = toIxs fun}
      newBuffDesc = Buff{buffName, buffRef}
  modify @BuffMap (BuffMap . Map.insert buffIndex newBuffDesc . buffMap)
  pure buffIndex

newtype Action = Action
  { runAction
      :: forall m sig
       . ( All sig m
         , Has (Reader BuffIndex) sig m
         , Has (Reader VarRef) sig m
         )
      => m ()
  }
instance Show Action where
  show _ = " [action] "

newtype BuffIndex = BuffIndex Int
  deriving (Show, Eq, Ord, Num)

newtype BuffName = BuffName String
  deriving (Show, Eq, Ord)

data Buff = Buff
  { buffName :: BuffName
  , buffRef :: BuffRef
  }
  deriving (Show)

newtype BuffMap = BuffMap {buffMap :: Map BuffIndex Buff}
  deriving (Show)

data TriggerInfo p = TriggerInfo
  { buffIndex :: BuffIndex
  , priority :: Int
  , triggerFun :: STrigger p -> Action
  , timesVarRef :: VarRef
  }

instance Show (TriggerInfo p) where
  show TriggerInfo{buffIndex, priority} =
    printf
      "trigger info: buffIndex %s, priority %d"
      (show buffIndex)
      priority

newtype TriggerMap = TriggerMap {triggerMap :: IntMap [TriggerInfo Any]}
  deriving (Show)

type All sig m =
  ( Has Random sig m
  , Has Fresh sig m
  , Has (Error GameError) sig m
  , Has (State Player) sig m
  , Has (State Game) sig m
  , Has (State VarMap) sig m
  , Has (State BuffMap) sig m
  , Has (State TriggerMap) sig m
  , HasLabelledLift IO sig m
  )

trigger
  :: forall p sig m
   . (All sig m, IX p)
  => STrigger p
  -> m ()
trigger t = do
  res <- get @TriggerMap
  case lookupTrigger @p (ix t) res of
    Nothing -> pure ()
    Just ts -> forM_ ts $
      \TriggerInfo{buffIndex, triggerFun, timesVarRef} -> do
        modifyVar timesVarRef (+ 1)
        catchError @GameError
          ( runReader timesVarRef
              . runReader buffIndex
              $ runAction (triggerFun t)
          )
          ( \case
              BuffEarlyExist -> pure ()
              e -> throwError e
          )

insertTrigger
  :: IX p
  => TriggerInfo p
  -> TriggerMap
  -> TriggerMap
insertTrigger
  t@(TriggerInfo{triggerFun})
  TriggerMap{triggerMap} =
    let index = ixF triggerFun
        nt = unsafeCoerce t :: TriggerInfo Any
     in TriggerMap $
          case IntMap.lookup index triggerMap of
            Nothing -> IntMap.insert index [nt] triggerMap
            Just v ->
              let nv = sortBy (\a b -> compare (priority a) (priority b)) $ nt : v
               in IntMap.insert index nv triggerMap

lookupTrigger
  :: IX p
  => Int
  -> TriggerMap
  -> Maybe [TriggerInfo p]
lookupTrigger index TriggerMap{triggerMap} =
  unsafeCoerce $ IntMap.lookup index triggerMap

deleteTrigger
  :: ( Has (State TriggerMap) sig m
     , Has (State VarMap) sig m
     )
  => BuffIndex
  -> Int
  -> m ()
deleteTrigger buffIndex index = do
  TriggerMap{triggerMap = trigM} <- get @TriggerMap
  case IntMap.lookup index trigM of
    Nothing -> pure ()
    Just vs -> do
      let collect v@TriggerInfo{buffIndex = p} (xs, ys) =
            if p == buffIndex then (v : xs, ys) else (xs, v : ys)
          (needDelet, remain) = foldr collect ([], []) vs
      forM_ needDelet $ \TriggerInfo{timesVarRef} -> deleteVar timesVarRef
      modify @TriggerMap (TriggerMap . IntMap.insert index remain . triggerMap)