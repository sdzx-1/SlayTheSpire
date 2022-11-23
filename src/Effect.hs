{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effect where

import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import qualified Control.Effect.Optics as O
import Optics (A_Getter, A_Setter, An_AffineFold, Is, Optic, Optic')
import Type

use
  :: forall s a m sig k is
   . (s ~ Game, Is k A_Getter, Has (State s) sig m)
  => Optic' k is s a
  -> m a
use = O.use

uses
  :: forall s a b m sig k is
   . (s ~ Game, Is k A_Getter, Has (State s) sig m)
  => Optic' k is s a
  -> (a -> b)
  -> m b
uses = O.uses

preuse
  :: forall s a m sig k is
   . (s ~ Game, Is k An_AffineFold, Has (State s) sig m)
  => Optic' k is s a
  -> m (Maybe a)
preuse = O.preuse

assign
  :: forall s a b m sig k is
   . (s ~ Game, Is k A_Setter, Has (State s) sig m)
  => Optic k is s s a b
  -> b
  -> m ()
assign = O.assign

modifying
  :: (s ~ Game, Is k A_Setter, Has (State s) sig m)
  => Optic k is s s a b
  -> (a -> b)
  -> m ()
modifying = O.modifying

infix 4 .=
infix 4 %=

(.=)
  :: forall s a b m sig k is
   . ( s ~ Game
     , Is k A_Setter
     , Has (State s) sig m
     )
  => Optic k is s s a b
  -> b
  -> m ()
(.=) = assign

(%=)
  :: ( s ~ Game
     , Is k A_Setter
     , Has (State s) sig m
     )
  => Optic k is s s a b
  -> (a -> b)
  -> m ()
(%=) = modifying
