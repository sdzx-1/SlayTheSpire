{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.T where

import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (runState)
import Control.Effect.Labelled (HasLabelledLift, lift, runLabelledLift)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.State (Has, State, get)
import Control.Monad (forM_, unless, void)
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import qualified Data.StateVar as SV
import Data.Word (Word8)
import Graphics.Type
import SDL (
  Event (eventPayload),
  EventPayload (KeyboardEvent, QuitEvent),
  InputMotion (Pressed),
  KeyboardEventData (KeyboardEventData, keyboardEventKeyMotion, keyboardEventKeysym),
  Keysym (..),
  Point (P),
  V2 (V2),
  V4 (V4),
  clear,
  copy,
  createRenderer,
  createTextureFromSurface,
  createWindow,
  defaultRenderer,
  defaultWindow,
  destroyWindow,
  freeSurface,
  initializeAll,
  pollEvents,
  present,
  ($=),
 )
import qualified SDL
import qualified SDL.Font as F
import qualified SDL.Framerate as F
import SDL.Input.Keyboard.Codes
import qualified SDL.Video.Renderer as R

main :: IO ()
main = do
  initializeAll
  F.initialize
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let rendererRef =
        RendererRef
          { renderer = renderer
          , rendererDrawBlendMode = R.rendererDrawBlendMode renderer
          , rendererDrawColor = R.rendererDrawColor renderer
          , rendererClipRect = R.rendererClipRect renderer
          , rendererScale = R.rendererScale renderer
          , rendererViewport = R.rendererViewport renderer
          }
  manager <- F.manager
  F.set manager 30
  void
    . runLabelledLift
    . runReader manager
    . runReader rendererRef
    $ runState wzh newLoop
  F.destroyManager manager
  destroyWindow window

newLoop
  :: ( HasLabelledLift IO sig m
     , Has (Reader F.Manager) sig m
     , Has (Reader RendererRef) sig m
     , Has (State WindowZipperHistory) sig m
     )
  => m ()
newLoop = do
  RendererRef{renderer} <- ask
  events <- lift pollEvents
  q <- lift $ do
    let eventIsQPress event =
          case eventPayload event of
            SDL.KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed
                && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            QuitEvent{} -> True
            _ -> False
        qPressed = any eventIsQPress events
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    pure qPressed
  forM_ events $ \event -> do
    case eventPayload event of
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeRight _)) -> mRight
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeLeft _)) -> mLeft
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeUp _)) -> mUp
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeDown _)) -> mDown
      _ -> pure ()
  wzh' <- get
  renderWindowZipperHistory
    (FatherArea (GlobalPos $ P (V2 0 0)) (V2 500 500))
    wzh'
  lift $ present renderer
  manager <- ask
  lift $ F.delay_ manager
  unless q newLoop

windows =
  Window
    { windowConfig =
        WindowConfig
          { area = createArea 0 0 750 580
          , backgroundColor = red
          }
    , children =
        fromJust $
          listToListZipper
            [ Window
                { windowConfig =
                    WindowConfig
                      { area = createArea 100 0 100 100
                      , backgroundColor = blue
                      }
                , children =
                    fromJust $
                      listToListZipper
                        [ Widget
                            { windowConfig =
                                WindowConfig
                                  { area = createArea 30 0 62 60
                                  , backgroundColor = red
                                  }
                            , widget = Blank
                            }
                        , Widget
                            { windowConfig =
                                WindowConfig
                                  { area = createArea 30 75 62 60
                                  , backgroundColor = red
                                  }
                            , widget = Blank
                            }
                        ]
                }
            , Window
                { windowConfig =
                    WindowConfig
                      { area = createArea 157 122 125 120
                      , backgroundColor = blue
                      }
                , children =
                    fromJust $
                      listToListZipper
                        [ Widget
                            { windowConfig =
                                WindowConfig
                                  { area = createArea 30 0 63 60
                                  , backgroundColor = red
                                  }
                            , widget = Blank
                            }
                        ]
                }
            ]
    }

-- w1 =
--   Leaf
--     { position = (80, 10)
--     , size = (150, 140)
--     , widget = Blank
--     }

wzh =
  WindowZipperHistory
    { windowDiffInfos = []
    , windowFocus = windows
    }