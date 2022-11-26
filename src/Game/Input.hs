{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Input where

import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import Control.Monad (forever)
import Data.Dynamic
import Data.Kind
import qualified Data.Map as Map
import Game.Type (Index)
import Text.Read (readMaybe)

data Avi t = Avi
  { title :: String
  , value :: [(Index, t, String)]
  }
  deriving (Show)

data AvailableList (xs :: [Type]) where
  ANil :: AvailableList '[]
  (:+)
    :: (Show t, Typeable t)
    => Avi t
    -> AvailableList xs
    -> AvailableList (t ': xs)

infixr 5 :+

instance Show (AvailableList xs) where
  show ANil = ""
  show (a :+ ANil) = show a
  show (a :+ ls) = show a ++ ", " ++ show ls

data ResultList (xs :: [Type]) where
  RNil :: ResultList '[]
  (:-) :: (Show t, Typeable t) => t -> ResultList xs -> ResultList (t ': xs)

infixr 5 :-

instance Show (ResultList xs) where
  show RNil = ""
  show (a :- RNil) = show a
  show (a :- ls) = show a ++ ", " ++ show ls

returnLevel
  :: forall sig m
   . ( Has (Error Control) sig m
     , Has (State [Dynamic]) sig m
     , HasLabelledLift IO sig m
     )
  => Int
  -> m ()
  -> m ()
returnLevel i m =
  catchError @Control
    ( do
        let bcst = "0 RETURN TO UPPER LEVEL"
        lift $ putStrLn bcst
        m
    )
    ( \case
        Level vi | vi == i -> do
          modify @[Dynamic] (drop 1)
          pure ()
        e -> throwError e
    )

data Control
  = Finish
  | Level Int

go
  :: forall xs sig m
   . ( Has (Error Control) sig m
     , Has (State [Dynamic]) sig m
     , HasLabelledLift IO sig m
     )
  => AvailableList xs
  -> Int
  -> m ()
go ANil _ = throwError Finish
go ((Avi title' ts) :+ tss) levelIndex =
  forever . returnLevel levelIndex $ do
    let st = "--> " ++ title'
    lift $ putStrLn $ unlines $ st : map (\(a, _, c) -> show a ++ "-" ++ c) ts
    mbi <- readMaybe @Int <$> lift getLine
    case mbi of
      Nothing -> lift $ putStrLn "Input error, retry"
      Just 0 -> throwError $ Level (levelIndex - 1)
      Just index -> do
        case Map.lookup index (Map.fromList (map (\(a, b, _) -> (a, b)) ts)) of
          Nothing -> lift $ putStrLn "Input out of range, retry"
          Just v -> do
            modify @[Dynamic] (toDyn v :)
            go tss (levelIndex + 1)

class TRS xs where
  trs :: [Dynamic] -> Maybe (ResultList xs)

instance {-# OVERLAPPABLE #-} TRS '[] where
  trs [] = Just RNil
  trs _ = Nothing

instance {-# OVERLAPPABLE #-} (TRS xs, Typeable x, Show x) => TRS (x : xs) where
  trs [] = Nothing
  trs (x : xs) = do
    x' <- fromDynamic x
    xs' <- trs xs
    pure $ x' :- xs'

getInput
  :: forall xs sig m
   . ( HasLabelledLift IO sig m
     , TRS xs
     )
  => AvailableList xs
  -> m (Maybe (ResultList xs))
getInput avl = do
  (dys, e) <- runState @[Dynamic] [] $ runError @Control $ go avl 0
  case e of
    Right () -> pure Nothing
    Left (Level _) -> pure Nothing
    Left Finish -> do
      let ndys = reverse dys
      pure (trs ndys)

-- >>> show te
-- "[(1,1),(2,2),(3,3)], [(1,True),(2,False)], [(1,True),(2,False)], [(1,1),(2,2),(3,3)], "
te :: AvailableList '[Int, Bool, Bool, Int]
te =
  Avi "nice" [(1, 1, "1"), (2, 2, "2"), (3, 3, "3")]
    :+ Avi "hello" [(1, True, "True"), (2, False, "False")]
    :+ Avi "hello" [(1, True, "True"), (2, False, "False")]
    :+ Avi "hello" [(1, 1, "1"), (2, 2, "2"), (3, 3, "3")]
    :+ ANil

tf =
  runLabelledLift $ getInput te