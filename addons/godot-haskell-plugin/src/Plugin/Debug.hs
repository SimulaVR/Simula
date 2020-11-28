{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugin.Debug where

import Control.Concurrent
import System.Posix.Process
import System.Process
import qualified System.Process.ByteString as B
import qualified Data.ByteString.Char8 as B
import System.Process.Internals
import System.Posix.Types
import System.Posix.Signals
import System.Directory
import System.Exit
import System.Timeout
import           Data.Bits
import           Linear
import           Plugin.Imports
import Data.Colour
import Data.Colour.SRGB.Linear

import Godot.Core.GodotVisualServer          as G
import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript

import qualified Data.Map.Strict as M

import Data.UUID
import Data.UUID.V1

import Plugin.Input
import Plugin.SimulaViewSprite
import Plugin.Types

import Control.Monad
import Control.Concurrent
import System.Environment

import System.Process
import GHC.IO.Handle

import Telemetry

import           Debug.Trace
import           Control.Lens hiding (Context)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Data.Maybe
import           Data.List
import           Data.Coerce
import           Unsafe.Coerce
import           Data.Either

import           Foreign hiding (void)
import           Foreign.C.Error
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Plugin.CanvasBase
import           Plugin.CanvasSurface

import           System.Clock
import           Control.Monad.Extra

import Godot.Core.GodotGlobalConstants as G
import Godot.Core.GodotInput as G
import Dhall
import Control.Exception

import Test.Hspec hiding (focus)
import Test.QuickCheck hiding ((.&.))
import Test.Hspec.Core.Runner
import Test.Hspec.Formatters

debugLaunchApp :: GodotSimulaServer -> String -> [String] -> IO GodotSimulaViewSprite
debugLaunchApp gss app args = do
  appLaunch gss app []
  gsvs <- waitUntilAppLaunchSuccessful gss
  return gsvs

  where waitUntilAppLaunchSuccessful :: GodotSimulaServer -> IO GodotSimulaViewSprite
        waitUntilAppLaunchSuccessful gss = do
          Control.Concurrent.threadDelay (round (0.2 * 1000000)) -- HACK: Need some other blocking IO call to induce other threads to run/the gsvs to actually get launched
          maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
          gsvs <- case maybeActiveCursorGSVS of
                      Nothing -> waitUntilAppLaunchSuccessful gss
                      Just gsvs -> return gsvs
          return gsvs

debugWaitFrames :: GodotSimulaViewSprite -> Integer -> IO ()
debugWaitFrames gsvs n = do
  gsvsOriginalFrameCount <- readTVarIO (gsvs ^. gsvsFrameCount)
  debugWaitFrames' gsvs gsvsOriginalFrameCount n
  where debugWaitFrames' :: GodotSimulaViewSprite -> Integer -> Integer -> IO ()
        debugWaitFrames' gsvs orig n = do
          frameCount <- readTVarIO (gsvs ^. gsvsFrameCount)
          Control.Concurrent.threadDelay (round (0.2 * 1000000)) -- HACK: Need some other blocking IO call to induce other threads to run/the gsvs to actually get launched
          case (frameCount >= (orig + n)) of
            False -> debugWaitFrames' gsvs orig n
            True -> return ()

-- https://docs.godotengine.org/en/stable/classes/class_@globalscope.html#enum-globalscope-buttonlist
debugMouseClick :: Int -> Bool -> IO ()
debugMouseClick button pressed = do
  a <- unsafeInstance GodotInputEventMouseButton "InputEventMouseButton"
  G.set_button_index a button
  -- G.position a =<< toLowLevel V2 520 520
  G.set_pressed a pressed
  getSingleton GodotInput "Input" >>= \inp -> G.parse_input_event inp ((safeCast a) :: GodotInputEvent)

debugLeftClick :: IO ()
debugLeftClick = do
  debugMouseClick 1 True
  debugMouseClick 1 False

debugRightClick :: IO ()
debugRightClick = do
  debugMouseClick 2 True
  debugMouseClick 2 False

debugKeyboardPress :: Scancode -> Bool -> IO ()
debugKeyboardPress button pressed = do
  a <- unsafeInstance GodotInputEventKey "InputEventKey"
  G.set_scancode a button
  G.set_pressed a pressed
  getSingleton GodotInput "Input" >>= \inp -> G.parse_input_event inp ((safeCast a) :: GodotInputEvent)

debugMoveCursor :: GodotSimulaViewSprite -> (Float, Float) -> IO ()
debugMoveCursor gsvs (sx, sy) = do
  processClickEvent' gsvs Motion (SurfaceLocalCoordinates (sx, sy))
  atomically $ writeTVar (gsvs ^. gsvsCursorCoordinates) (SurfaceLocalCoordinates (sx, sy))

debugTerminateSimula :: GodotSimulaServer -> IO ()
debugTerminateSimula gss = do
  let pid = (gss ^. gssPid)
  -- logStr $ "Terminating Simula with pid: " ++ (show pid)
  createProcess (shell $ "kill " ++ (show pid))
  return ()

testRightclickPopup :: GodotSimulaServer -> String -> ScreenshotBaseName -> IO ((Float, Float), (Float, Float), ScreenshotFullPath)
testRightclickPopup gss app screenshotBase = do
  Control.Concurrent.threadDelay (2 * 1000000)
  gsvs <- debugLaunchApp gss app []
  debugMoveCursor gsvs (300,300)
  Control.Concurrent.threadDelay (round (0.5 * 1000000))
  debugMouseClick 2 True
  debugMouseClick 2 False
  Control.Concurrent.threadDelay (5 * 1000000)
  screenshotFullPath <- savePngPancake gss screenshotBase

  SurfaceLocalCoordinates (cx, cy) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurfaceParent <- getWlrSurface eitherSurface
  depthFirstBaseSurfaces <- case eitherSurface of
    Left wlrXdgSurface -> getDepthFirstXdgSurfaces wlrXdgSurface :: IO [(GodotWlrSurface, Int, Int)]
    Right wlrXWaylandSurface -> getDepthFirstXWaylandSurfaces wlrXWaylandSurface :: IO [(GodotWlrSurface, Int, Int)]
  depthFirstWlrSurfaces <- getDepthFirstWlrSurfaces wlrSurfaceParent
  let depthFirstSurfaces = depthFirstBaseSurfaces ++ depthFirstWlrSurfaces
  let len = Data.List.length depthFirstSurfaces
  freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
  popup@(popupWlrSurface, px, py) <- case (Data.List.length freeChildren > 0, (len >= 2)) of
                                             (True, _) -> do let freeChild = Data.List.head freeChildren
                                                             wlrSurface <- G.get_wlr_surface freeChild
                                                             x <- G.get_x freeChild
                                                             y <- G.get_y freeChild
                                                             return (wlrSurface, x, y)
                                             (_, True) -> return $ depthFirstSurfaces !! 1
                                             _ -> return $ depthFirstSurfaces !! 0

  -- debugTerminateGSVS gsvs -- Suffers from threading issues; use pkill instead
  createProcess (shell $ "pkill " ++ app)
  return ((cx, cy), (fromIntegral px, fromIntegral py), screenshotFullPath)

debugTerminateApps :: GodotSimulaServer -> IO ()
debugTerminateApps gss = do
  views <- readTVarIO (gss ^. gssViews)
  let gsvsLst = fmap snd (M.toList views)
  mapM_ debugTerminateGSVS gsvsLst
  return ()
  where snd (a, b) = b

-- | Suffers from threading issues, even with liberal threadDelay hack calls.
debugTerminateGSVS :: GodotSimulaViewSprite -> IO ()
debugTerminateGSVS gsvs = do
  Control.Concurrent.threadDelay (2 * 1000000)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  case eitherSurface of
    (Left wlrXdgSurface) -> do
      toplevel  <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
      G.send_close toplevel
    (Right wlrXWaylandSurface) -> do
      G.send_close wlrXWaylandSurface
  waitUntilAppCloseSuccessful gss
  where waitUntilAppCloseSuccessful :: GodotSimulaServer -> IO ()
        waitUntilAppCloseSuccessful gss = do
          Control.Concurrent.threadDelay (2 * 1000000)
          maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
          case maybeActiveCursorGSVS of
               Nothing -> return ()
               Just gsvs -> do
                 Control.Concurrent.threadDelay (2 * 1000000)
                 waitUntilAppCloseSuccessful gss


testPopups :: GodotSimulaServer -> IO ()
testPopups gss = do
  let config = defaultConfig { configOutputFile = Right $ "./hspec_output.txt" }
  (cursorCoordsGedit, popupCoordsGedit, geditScreenshot) <- testRightclickPopup gss "google-chrome-stable" "google-chrome"
  hspecWith config $ do
    describe "A popup initiated at cursor location (300,300)" $ do
        it ("gedit cursor coordinates should be same as popup location\n\n[[" <> geditScreenshot <> "]]\n\n") $ do
           popupCoordsGedit  `shouldBe` cursorCoordsGedit
  return ()

debugFunc :: GodotSimulaServer -> IO ()
debugFunc gss = do
  (catch :: IO a -> (System.Exit.ExitCode -> IO a) -> IO a) (do testPopups gss
                                                                debugTerminateSimula gss)
                                                            (\e -> debugTerminateSimula gss)
  return ()