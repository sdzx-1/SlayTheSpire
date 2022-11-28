{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MutableVector where

import Control.Effect.Labelled (HasLabelledLift, lift, runLabelledLift)
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

-- Variable not in scope: runF
{--
runF = runST $ runLabelledLift f
f
  :: ( HasLabelledLift n sig m
     , MutableVector n
     )
  => m Int
f = do
  mv <- replicate 10 1
  write mv 1 (10 :: Int)
  write mv 2 (10 :: Int)
  exchange mv 3 10
--}
