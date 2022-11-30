{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Graphics.Type where

import Control.Carrier.Reader (runReader)
import Control.Effect.Labelled (Has, HasLabelledLift, lift)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.State (State, get, put)
import Control.Monad (void)
import Data.Foldable (foldlM)
import Data.StateVar (StateVar)
import qualified Data.StateVar as SV
import Data.Word (Word8)
import Deque.Strict hiding (drop, reverse)
import Foreign.C.Types (CFloat, CInt)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import SDL (Point (P), V2 (..), V4 (..), origin)
import qualified SDL
import SDL.Video.Renderer (
  BlendMode,
  Rectangle (..),
  Renderer,
 )

data RendererRef = RendererRef
  { renderer :: Renderer
  , rendererDrawBlendMode :: StateVar BlendMode
  , rendererDrawColor :: StateVar (V4 Word8)
  , rendererClipRect :: StateVar (Maybe (Rectangle CInt))
  , rendererScale :: StateVar (V2 CFloat)
  , rendererViewport :: StateVar (Maybe (Rectangle CInt))
  }

data LocalPos = LocalPos (Point V2 Double)
  deriving (Show)

data GlobalPos = GlobalPos (Point V2 Double)
  deriving (Show)

createArea :: Double -> Double -> Double -> Double -> Area
createArea a b c d = Area (LocalPos (P (V2 a b))) (V2 c d)

data Area = Area
  { position :: LocalPos
  , size :: V2 Double
  }
  deriving (Show)

data Origin = Origin GlobalPos
  deriving (Show)

data FatherArea = FatherArea
  { position :: GlobalPos
  , size :: V2 Double
  }
  deriving (Show)

data WindowConfig = WindowConfig
  { area :: Area
  , backgroundColor :: V4 Word8
  }
  deriving (Show)

data Window
  = Window
      { windowConfig :: WindowConfig
      , children :: ListZipper Window
      }
  | forall a.
    (Render a, Responder a) =>
    Widget
      { windowConfig :: WindowConfig
      , widget :: a
      }

getWindowConfig :: Window -> WindowConfig
getWindowConfig (Window{windowConfig}) = windowConfig
getWindowConfig (Widget{windowConfig}) = windowConfig

instance Show Window where
  show (Window c ch) = "config: " ++ show c ++ ", chilren: " ++ show ch
  show (Widget c _) = "config: " ++ show c ++ ", widget"

data ListZipper a = ListZipper
  { left :: Deque a
  , focus :: a
  , right :: Deque a
  }
  deriving (Show, Generic)

data WindowDiffInfos = WindowDiffInfos
  { windowLeft :: Deque Window
  , windowRight :: Deque Window
  , windowConfig :: WindowConfig
  }
  deriving (Show, Generic)

data WindowZipperHistory = WindowZipperHistory
  { windowDiffInfos :: [WindowDiffInfos]
  , windowFocus :: Window
  }
  deriving (Show, Generic)

--------------------------------------
class Render a where
  render
    :: ( HasLabelledLift IO sig m
       , Has (Reader FatherArea) sig m
       , Has (Reader RendererRef) sig m
       )
    => a
    -> m ()

class Responder a

instance Render Window where
  render Window{windowConfig = WindowConfig{area, backgroundColor}, children} = do
    void $
      renderSub
        area
        ( \fa -> runReader fa $ do
            let ls = listZipperToList children
            mapM_ render ls
            withColor backgroundColor renderFatherArea
        )
  render Widget{windowConfig = WindowConfig{area, backgroundColor}, widget} = do
    void $
      renderSub
        area
        ( \fa -> runReader fa $ do
            render widget
            withColor backgroundColor renderFatherArea
        )

renderSub
  :: ( HasLabelledLift IO sig m
     , Has (Reader FatherArea) sig m
     , Has (Reader RendererRef) sig m
     )
  => Area
  -> (FatherArea -> m ())
  -> m (Maybe FatherArea)
renderSub area m = do
  fa@(FatherArea ps _) <- ask
  let newFatherArea = toFatherArea area ps
  case intersection fa newFatherArea of
    Nothing -> pure Nothing
    Just nfa -> do
      -- (m nfa)
      -- lift $ print nfa
      withClip nfa (m nfa)
      pure (Just nfa)

toFatherArea :: Area -> GlobalPos -> FatherArea
toFatherArea (Area (LocalPos ps) sz) (GlobalPos gps) =
  FatherArea (GlobalPos (gps + ps)) sz

renderFatherArea
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     , Has (Reader FatherArea) sig m
     )
  => m ()
renderFatherArea = do
  (FatherArea ps sz) <- ask
  runReader (Origin ps) $
    drawRect (Area (LocalPos origin) sz)

intersection :: FatherArea -> FatherArea -> Maybe FatherArea
intersection
  (FatherArea (GlobalPos (P p0)) v0)
  (FatherArea (GlobalPos (P p1@(V2 x1 y1))) v1) =
    let V2 x0' y0' = p0 + v0
        V2 x1' y1' = p1 + v1
     in if x1 >= x0' || y1 >= y0'
          then Nothing
          else
            Just $
              FatherArea
                (GlobalPos (P (V2 x1 y1)))
                (V2 (min x0' x1' - x1) (min y0' y1' - y1))

--------------------------------------

renderWindowZipperHistory
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     )
  => FatherArea
  -> WindowZipperHistory
  -> m ()
renderWindowZipperHistory
  fatherArea@FatherArea{position = GlobalPos v, size = sz}
  WindowZipperHistory
    { windowDiffInfos
    , windowFocus
    } = do
    RendererRef{rendererClipRect} <- ask
    lift $
      rendererClipRect
        SV.$= Just
          ( Rectangle
              (fmap truncate v)
              (fmap truncate sz)
          )
    let revs = reverse windowDiffInfos
    nfa <-
      foldlM
        ( \mfa
           WindowDiffInfos
            { windowConfig =
              WindowConfig
                { area
                , backgroundColor
                }
            , windowLeft
            , windowRight
            } -> case mfa of
              Nothing -> pure Nothing
              Just fa -> do
                runReader fa $ do
                  renderSub area $ \nfa' -> do
                    runReader nfa' $ do
                      withColor backgroundColor renderFatherArea
                      mapM_ render windowLeft
                      mapM_ render windowRight
        )
        (Just fatherArea)
        revs
    case nfa of
      Nothing -> pure ()
      Just fa@(FatherArea{position = ps}) -> runReader fa $ do
        render windowFocus
        let WindowConfig{area} = getWindowConfig windowFocus
        let nfa' = toFatherArea area ps
        runReader nfa' $ render Blank1

--------------------------------------

red, green, blue, black :: V4 Word8
red = V4 255 0 0 255
green = V4 0 255 0 255
blue = V4 0 0 255 255
black = V4 0 0 0 255

data Blank = Blank

instance Render Blank where
  render _ = do
    FatherArea{position = sp} <- ask
    withColor (V4 0 0 0 255) $ do
      runReader (Origin sp) $ fillRect (createArea 0 0 10 10)

instance Responder Blank

data Blank1 = Blank1

instance Render Blank1 where
  render _ = do
    FatherArea{position = sp} <- ask
    withColor blue $ do
      runReader (Origin sp) $ fillRect (createArea 0 0 10 10)

instance Responder Blank1

--------------------------------------
moveLeft :: ListZipper a -> ListZipper a
moveLeft ListZipper{left, focus, right} =
  case unsnoc left of
    Nothing ->
      let newRight = cons focus right
       in case unsnoc newRight of
            Nothing -> error "undefined befined"
            Just (x, xs) ->
              ListZipper
                { left = xs
                , focus = x
                , right = []
                }
    Just (x, xs) ->
      ListZipper
        { left = xs
        , focus = x
        , right = cons focus right
        }

moveRight :: ListZipper a -> ListZipper a
moveRight ListZipper{left, focus, right} =
  case uncons right of
    Nothing ->
      let newLeft = snoc focus left
       in case uncons newLeft of
            Nothing -> error "undefined befined"
            Just (x, xs) ->
              ListZipper
                { left = []
                , focus = x
                , right = xs
                }
    Just (x, xs) ->
      ListZipper
        { left = snoc focus left
        , focus = x
        , right = xs
        }

listToListZipper :: [a] -> Maybe (ListZipper a)
listToListZipper [] = Nothing
listToListZipper (x : xs) = Just $ ListZipper [] x (fromList xs)

listZipperToList :: ListZipper a -> [a]
listZipperToList ListZipper{left, focus, right} =
  toList $ snoc focus left <> right

moveFun
  :: Has (State WindowZipperHistory) sig m
  => (ListZipper Window -> ListZipper Window)
  -> m ()
moveFun mFun = do
  WindowZipperHistory{windowDiffInfos, windowFocus} <- get
  case windowDiffInfos of
    [] -> pure ()
    wd@WindowDiffInfos{windowLeft, windowRight} : xs -> do
      let ListZipper{left, focus, right} = mFun $ ListZipper windowLeft windowFocus windowRight
      put
        WindowZipperHistory
          { windowDiffInfos =
              wd
                { windowLeft = left
                , windowRight = right
                }
                : xs
          , windowFocus = focus
          }

mLeft :: Has (State WindowZipperHistory) sig m => m ()
mLeft = moveFun moveLeft

mRight :: Has (State WindowZipperHistory) sig m => m ()
mRight = moveFun moveRight

mUp :: Has (State WindowZipperHistory) sig m => m ()
mUp = do
  WindowZipperHistory{windowDiffInfos, windowFocus} <- get
  case windowDiffInfos of
    WindowDiffInfos{windowLeft, windowRight, windowConfig} : xs -> do
      put
        WindowZipperHistory
          { windowFocus =
              Window
                { windowConfig
                , children =
                    ListZipper
                      { left = windowLeft
                      , focus = windowFocus
                      , right = windowRight
                      }
                }
          , windowDiffInfos = xs
          }
    _ -> pure ()

mDown :: Has (State WindowZipperHistory) sig m => m ()
mDown = do
  WindowZipperHistory
    { windowDiffInfos
    , windowFocus
    } <-
    get
  case windowFocus of
    Widget{} -> pure ()
    Window{windowConfig, children = ListZipper{left, focus, right}} -> do
      put
        WindowZipperHistory
          { windowFocus = focus
          , windowDiffInfos =
              WindowDiffInfos
                { windowLeft = left
                , windowRight = right
                , windowConfig
                }
                : windowDiffInfos
          }

-------------------------

drawLine
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     , Has (Reader Origin) sig m
     )
  => LocalPos
  -> LocalPos
  -> m ()
drawLine (LocalPos start) (LocalPos end) = do
  RendererRef{renderer} <- ask
  (Origin (GlobalPos sp)) <- ask
  let start' = fmap truncate $ sp + start
      end' = fmap truncate $ sp + end
  lift $ SDL.drawLine renderer start' end'

drawRect
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     , Has (Reader Origin) sig m
     )
  => Area
  -> m ()
drawRect (Area (LocalPos p) v) = do
  RendererRef{renderer} <- ask
  (Origin (GlobalPos sp)) <- ask
  let p' = fmap truncate $ sp + p
      v' = truncate <$> v
  lift $ SDL.drawRect renderer (Just $ Rectangle p' v')

fillRect
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     , Has (Reader Origin) sig m
     )
  => Area
  -> m ()
fillRect (Area (LocalPos p) v) = do
  RendererRef{renderer} <- ask
  (Origin (GlobalPos sp)) <- ask
  let p' = fmap truncate $ sp + p
      v' = truncate <$> v
  lift $ SDL.fillRect renderer (Just $ Rectangle p' v')

withHelp
  :: ( Has (Reader RendererRef) sig m
     , HasLabelledLift IO sig m
     )
  => StateVar a
  -> a
  -> m b
  -> m b
withHelp sv a m = do
  crBackup <- lift $ SV.get sv
  lift $ sv SV.$= a
  res <- m
  lift $ sv SV.$= crBackup
  pure res

withColor
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     )
  => V4 Word8
  -> m a
  -> m a
withColor color m = do
  RendererRef{rendererDrawColor} <- ask
  withHelp rendererDrawColor color m

withClip
  :: ( HasLabelledLift IO sig m
     , Has (Reader RendererRef) sig m
     )
  => FatherArea
  -> m a
  -> m a
withClip
  ( FatherArea
      { position = GlobalPos pos
      , size
      }
    )
  m = do
    RendererRef{rendererClipRect} <- ask
    let pos' = fmap truncate pos
        size' = fmap truncate size
    withHelp rendererClipRect (Just $ Rectangle pos' size') m
