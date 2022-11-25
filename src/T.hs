{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module T where

import Control.Algebra (Has)
import Control.Effect.Error (Error)
import Control.Effect.Labelled (HasLabelledLift)
import Control.Effect.Random (Random)
import Control.Effect.State (State, get, modify)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind
import GHC.Exts (Any)
import GHC.TypeLits
import Type (Action, Game, GameError, runAction)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

data Trigger
  = PlayerTakesDamage
  | PlayerDies
  | EnemyDies
  | PlayerSelectDefends
  | PlayerSelectAttacks
  | NewTurnStart
  deriving (Eq, Show, Ord)

data STrigger (p :: Trigger) where
  SPlayerTakesDamage :: STrigger PlayerTakesDamage
  SPlayerDies :: STrigger PlayerDies
  SEnemyDies :: {remainAttack :: Int, enemyId :: Int} -> STrigger EnemyDies
  SPlayerSelectDefends :: STrigger PlayerSelectDefends
  SPlayerSelectAttacks :: STrigger PlayerSelectAttacks
  SNewTurnStart :: STrigger NewTurnStart

class IX (p :: Trigger) where
  ix :: STrigger p -> Int
  ixF :: (STrigger p -> Action) -> Int

ixT :: Trigger -> Int
ixT PlayerTakesDamage = 1
ixT PlayerDies = 2
ixT EnemyDies = 3
ixT PlayerSelectDefends = 4
ixT PlayerSelectAttacks = 5
ixT NewTurnStart = 6

instance IX 'PlayerTakesDamage where
  ix _ = 1
  ixF _ = 1
instance IX 'PlayerDies where
  ix _ = 2
  ixF _ = 2
instance IX 'EnemyDies where
  ix _ = 3
  ixF _ = 3
instance IX 'PlayerSelectDefends where
  ix _ = 4
  ixF _ = 4
instance IX 'PlayerSelectAttacks where
  ix _ = 5
  ixF _ = 5
instance IX 'NewTurnStart where
  ix _ = 6
  ixF _ = 6

data HList (xs :: [Trigger]) where
  HNil :: HList '[]
  (:::) :: (STrigger x -> Action) -> HList xs -> HList (x ': xs)

infixr 5 :::

data Buff = BuffA

putFun
  :: ( Has Random sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State Game) sig m
     , Has (State AMap) sig m
     )
  => AnyAction
  -> m ()
putFun (AnyAction xs) = do
  xs' <- xs
  modify @AMap (insertAMap xs')

class ToIxs (xs :: [Trigger]) where
  toIxs :: HList xs -> [Int]

instance ToIxs '[] where
  toIxs HNil = [0]

instance (IX x, ToIxs xs) => ToIxs (x ': xs) where
  toIxs (a ::: b) = ixF a : toIxs b

type O = '[PlayerDies, NewTurnStart]
o :: HList O
o = undefined ::: undefined ::: HNil
type O' = '[PlayerDies]
o' :: HList O'
o' = undefined ::: HNil

data AnyAction
  = forall xs.
    InsertAMap xs =>
    AnyAction
      ( forall sig m
         . ( Has Random sig m
           , Has (Error GameError) sig m
           , HasLabelledLift IO sig m
           , Has (State Game) sig m
           , Has (State AMap) sig m
           )
        => m (HList xs)
      )

bb :: [AnyAction]
bb = [AnyAction iif, AnyAction iif']

iif'
  :: ( Has Random sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State Game) sig m
     , Has (State AMap) sig m
     )
  => m (HList O')
iif' = pure o'

iif
  :: ( Has Random sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State Game) sig m
     , Has (State AMap) sig m
     )
  => m (HList O)
iif = pure o

-- >>> ii
ii = toIxs o

data TriggerInfo = TriggerInfo
  { index :: Int
  , priority :: Int
  , fun :: STrigger Any -> Action
  }

type ActionList = [TriggerInfo]

type AMap = IntMap (STrigger Any -> Action)

class InsertAMap xs where
  insertAMap :: HList xs -> AMap -> AMap

instance InsertAMap '[] where
  insertAMap HNil am = am

instance (IX x, InsertAMap xs) => InsertAMap (x ': xs) where
  insertAMap (a ::: b) am = insertAMap b $ insert a am

insert :: IX p => (STrigger p -> Action) -> AMap -> AMap
insert f am =
  let index = ixF f
   in IntMap.insert index (unsafeCoerce f) am

lookup :: IX p => Int -> AMap -> Maybe (STrigger p -> Action)
lookup i am = unsafeCoerce $ IntMap.lookup i am

trigger
  :: ( Has Random sig m
     , Has (Error GameError) sig m
     , HasLabelledLift IO sig m
     , Has (State Game) sig m
     , Has (State AMap) sig m
     , IX p
     )
  => STrigger p
  -> m ()
trigger t = do
  f <- lookup (ix t) <$> get @AMap
  case f of
    Nothing -> pure ()
    Just f' -> runAction $ f' t

-- f1 :: SPoint PointTheEnemyDies -> Action
-- f1 _ = undefined

-- -- f :: SPoint PointPlayerDies -> Action
-- f (SPointPlayerDies{remainAttack, enemyIndex}) = undefined

-- tri' s@(SPointPlayerDies{remainAttack = 10, enemyIndex = 1}) = undefined