{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.VarMap where

import Control.Algebra (Has)
import Control.Effect.Fresh (Fresh, fresh)
import Control.Effect.Optics (assign, modifying, use)
import Control.Effect.State (State)
import Data.Map (Map)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics (At (at), makeFieldLabels, (%), (%?))

newtype VarRef = VarRef Int deriving (Show, Generic, Eq, Ord)

newtype VarMap = VarMap {varMap :: Map VarRef Int}
  deriving (Show, Generic)

makeFieldLabels ''VarMap

definedVar
  :: ( Has (State VarMap) sig m
     , Has Fresh sig m
     )
  => Int
  -> m VarRef
definedVar initValue = do
  nVar <- VarRef <$> fresh
  assign @VarMap (#varMap % at nVar) (Just initValue)
  pure nVar

useVar
  :: ( Has (State VarMap) sig m
     )
  => VarRef
  -> m Int
useVar ref = do
  use @VarMap (#varMap % at ref) >>= \case
    Nothing -> error "undefined behave"
    Just i -> pure i

assignVar
  :: ( Has (State VarMap) sig m
     )
  => VarRef
  -> Int
  -> m ()
assignVar varRef val =
  assign @VarMap (#varMap % at varRef) (Just val)

modifyVar
  :: ( Has (State VarMap) sig m
     )
  => VarRef
  -> (Int -> Int)
  -> m ()
modifyVar varRef fun = do
  modifying @_ @VarMap (#varMap % at varRef) (Just . fun . fromJust)

deleteVar
  :: ( Has (State VarMap) sig m
     )
  => VarRef
  -> m ()
deleteVar varRef =
  assign @VarMap (#varMap % at varRef) Nothing