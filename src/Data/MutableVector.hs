{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.MutableVector where

import Control.Carrier.Reader (Reader, runReader)
import Control.Effect.Labelled (HasLabelled, HasLabelledLift, lift, runLabelled, runLabelledLift)
import qualified Control.Effect.Reader.Labelled as L
import Control.Monad.ST (ST, runST)
import Data.Vector.Generic.Mutable (PrimMonad (PrimState))
import qualified Data.Vector.Mutable as M
import Prelude hiding (read, replicate)

newtype MVector n a = MVector (M.MVector (PrimState n) a)

class MutableVector n where
  --- Length information
  length :: MVector n a -> Int
  null :: MVector n a -> Bool

  --- Slice
  slice :: Int -> Int -> MVector n a -> MVector n a

  --- Initialisation
  new :: HasLabelledLift n sig m => Int -> m (MVector n a)
  replicate :: HasLabelledLift n sig m => Int -> a -> m (MVector n a)
  generate :: HasLabelledLift n sig m => Int -> (Int -> a) -> m (MVector n a)

  --- Growing
  grow :: HasLabelledLift n sig m => MVector n a -> Int -> m (MVector n a)

  --- Accessing individual elements
  read :: HasLabelledLift n sig m => MVector n a -> Int -> m a
  write :: HasLabelledLift n sig m => MVector n a -> Int -> a -> m ()
  modify :: HasLabelledLift n sig m => MVector n a -> (a -> a) -> Int -> m ()
  swap :: HasLabelledLift n sig m => MVector n a -> Int -> Int -> m ()
  exchange :: HasLabelledLift n sig m => MVector n a -> Int -> a -> m a

instance MutableVector (ST s) where
  --- Length information
  {-# INLINE length #-}
  length (MVector n) = M.length n
  {-# INLINE null #-}
  null (MVector n) = M.null n

  --- Initialisation
  {-# INLINE new #-}
  new n = lift $ MVector <$> M.new n
  {-# INLINE replicate #-}
  replicate a b = lift $ MVector <$> M.replicate a b
  {-# INLINE generate #-}
  generate a b = lift $ MVector <$> M.generate a b

  --- Slice
  {-# INLINE slice #-}
  slice a b (MVector n) = MVector $ M.slice a b n

  --- Growing
  {-# INLINE grow #-}
  grow (MVector a) b = lift $ MVector <$> M.grow a b

  --- Accessing individual elements
  {-# INLINE read #-}
  read (MVector n) a = lift $ M.read n a
  {-# INLINE write #-}
  write (MVector n) a b = lift $ M.write n a b
  {-# INLINE modify #-}
  modify (MVector n) a b = lift $ M.modify n a b
  {-# INLINE swap #-}
  swap (MVector m) a b = lift $ M.swap m a b
  {-# INLINE exchange #-}
  exchange (MVector n) a b = lift $ M.exchange n a b

instance MutableVector IO where
  --- Length information
  {-# INLINE length #-}
  length (MVector n) = M.length n
  {-# INLINE null #-}
  null (MVector n) = M.null n

  --- Initialisation
  {-# INLINE new #-}
  new n = lift $ MVector <$> M.new n
  {-# INLINE replicate #-}
  replicate a b = lift $ MVector <$> M.replicate a b
  {-# INLINE generate #-}
  generate a b = lift $ MVector <$> M.generate a b

  --- Slice
  {-# INLINE slice #-}
  slice a b (MVector n) = MVector $ M.slice a b n

  --- Growing
  {-# INLINE grow #-}
  grow (MVector a) b = lift $ MVector <$> M.grow a b

  --- Accessing individual elements
  {-# INLINE read #-}
  read (MVector n) a = lift $ M.read n a
  {-# INLINE write #-}
  write (MVector n) a b = lift $ M.write n a b
  {-# INLINE modify #-}
  modify (MVector n) a b = lift $ M.modify n a b
  {-# INLINE swap #-}
  swap (MVector m) a b = lift $ M.swap m a b
  {-# INLINE exchange #-}
  exchange (MVector n) a b = lift $ M.exchange n a b

-- >>> runF
-- "hello wellcom"
runF = runST $ do
  mv <- M.new 10
  runLabelledLift $
    runReader (S (MVector mv) 1) $
      runLabelled @S f

data S n = S
  { smv :: MVector n String
  , si :: Int
  }

f
  :: ( HasLabelledLift n sig m
     , HasLabelled S (Reader (S n)) sig m
     , MutableVector n
     )
  => m String
f = do
  S{smv} <- L.ask @S
  write smv 1 "hello"
  write smv 2 " wellcom"
  h <- read smv 1
  w <- read smv 2
  pure (h ++ w)
