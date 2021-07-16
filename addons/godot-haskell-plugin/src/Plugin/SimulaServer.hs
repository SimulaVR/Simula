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

module Plugin.SimulaServer where

import Data.Char
import Data.Time.Clock
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
import Godot.Core.GodotEnvironment           as G
import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript

import qualified Data.Map.Strict as M

import Data.UUID
import Data.UUID.V1

import Plugin.Input
import Plugin.SimulaViewSprite
import Plugin.Types
import Plugin.Debug

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
import           Plugin.Debug

import           System.Clock
import           Control.Monad.Extra

import Godot.Core.GodotGlobalConstants as G
import Godot.Core.GodotInput as G
import Dhall
import Control.Exception
import qualified Data.Vector as V

getKeyboardAction :: GodotSimulaServer -> KeyboardShortcut -> KeyboardAction
getKeyboardAction gss keyboardShortcut = 
  case (keyboardShortcut ^. keyAction) of
    "moveCursor" -> moveCursor
    "clickLeft" -> leftClick
    "launchTerminal" -> shellLaunch gss "./result/bin/xfce4-terminal"
    "launchXrpa" -> launchXpra' gss
    "toggleGrabMode" -> toggleGrabMode'
    "launchHMDWebCam" -> launchHMDWebCam' gss
    "orientWindowTowardsGaze" -> orientWindowTowardsGaze
    "grabWindow" -> grabWindow gss
    "grabWindows" -> grabWindows gss
    "grabWorkspaces" -> grabWorkspaces gss
    "pushWindow" -> pushWindow
    "pullWindow" -> pullWindow
    "scaleWindowDown" -> scaleWindowDown
    "scaleWindowUp" -> scaleWindowUp
    "contractWindowHorizontally" -> horizontalContract
    "extendWindowHorizontally" -> horizontalExtend
    "contractWindowVertically" -> verticalContract
    "extendWindowVertically" -> verticalExtend
    "resizeWindowToDefaultSize" -> resizeWindowToDefaultSize
    "zoomOut" -> zoomOut
    "zoomIn" -> zoomIn
    "terminateWindow" -> terminateWindow
    "reloadConfig" -> reloadConfig
    "terminateSimula" -> terminateSimula
    "cycleEnvironment" -> cycleEnvironment gss
    "launchAppLauncher" -> shellLaunch gss "./result/bin/synapse"
    "textToSpeech" -> textToSpeech gss
    "decreaseTransparency" -> decreaseTransparency
    "increaseTransparency" -> increaseTransparency
    "toggleScreenshotMode" -> toggleScreenshotMode
    "takeScreenshotGlobal" -> takeScreenshotGlobal
    "debugLogDepthFirstSurfaces" -> debugLogDepthFirstSurfaces
    "debugPrint" -> debugPrint
    "workspace0" -> switchToWorkspace gss 0
    "workspace1" -> switchToWorkspace gss 1
    "workspace2" -> switchToWorkspace gss 2
    "workspace3" -> switchToWorkspace gss 3
    "workspace4" -> switchToWorkspace gss 4
    "workspace5" -> switchToWorkspace gss 5
    "workspace6" -> switchToWorkspace gss 6
    "workspace7" -> switchToWorkspace gss 7
    "workspace8" -> switchToWorkspace gss 8
    "workspace9" -> switchToWorkspace gss 9
    "sendToWorkspace0" -> sendToWorkspace gss 0
    "sendToWorkspace1" -> sendToWorkspace gss 1
    "sendToWorkspace2" -> sendToWorkspace gss 2
    "sendToWorkspace3" -> sendToWorkspace gss 3
    "sendToWorkspace4" -> sendToWorkspace gss 4
    "sendToWorkspace5" -> sendToWorkspace gss 5
    "sendToWorkspace6" -> sendToWorkspace gss 6
    "sendToWorkspace7" -> sendToWorkspace gss 7
    "sendToWorkspace8" -> sendToWorkspace gss 8
    "sendToWorkspace9" -> sendToWorkspace gss 9
    "rotateWorkspaceHorizontallyLeft" -> rotateWorkspaceHorizontally gss (0.15707963)
    "rotateWorkspaceHorizontallyRight" -> rotateWorkspaceHorizontally gss (-0.15707963)
    "rotateWorkspacesHorizontallyLeft" -> rotateWorkspacesHorizontally gss (0.15707963)
    "rotateWorkspacesHorizontallyRight" -> rotateWorkspacesHorizontally gss (-0.15707963)
    "toggleARMode" -> toggleARMode gss
    _ -> shellLaunch gss (keyboardShortcut ^. keyAction)

  where moveCursor :: SpriteLocation -> Bool -> IO ()
        moveCursor (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          updateCursorStateAbsolute gsvs sx sy
          sendWlrootsMotion gsvs
        moveCursor _ _ = return ()

        debugLogDepthFirstSurfaces:: SpriteLocation -> Bool -> IO ()
        debugLogDepthFirstSurfaces (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          Plugin.Debug.debugLogDepthFirstSurfaces gsvs
        debugLogDepthFirstSurfaces (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) False = do
          return ()
        debugLogDepthFirstSurfaces _ _ = return ()

        leftClick :: SpriteLocation -> Bool -> IO ()
        leftClick (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          updateCursorStateAbsolute gsvs sx sy
          sendWlrootsMotion gsvs
          processClickEvent' gsvs (Button True 1) coords -- BUTTON_LEFT = 1
        leftClick (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) False = do
          processClickEvent' gsvs (Button False 1) coords -- BUTTON_LEFT = 1
        leftClick _ _ = return ()

        grabWindow :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        grabWindow gss (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          keyboardGrabInitiate gss (GrabWindow gsvs undefined)
        grabWindow gss (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) False = do
          keyboardGrabLetGo gss (GrabWindow gsvs undefined)
        grabWindow _ _ _ = return ()

        grabWindows :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        grabWindows gss _ True = do
          maybeGrab <- readTVarIO (gss ^. gssGrab)
          case maybeGrab of
            Nothing -> keyboardGrabInitiate gss (GrabWindows undefined)
            _ -> return ()
        grabWindows gss _ False = do
          keyboardGrabLetGo gss (GrabWindows undefined)

        grabWorkspaces :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        grabWorkspaces gss _ True = do
          maybeGrab <- readTVarIO (gss ^. gssGrab)
          case maybeGrab of
            Nothing -> keyboardGrabInitiate gss (GrabWorkspaces undefined)
            _ -> return ()
        grabWorkspaces gss _ False = do
          keyboardGrabLetGo gss (GrabWorkspaces undefined)

        rotateWorkspaceHorizontally :: GodotSimulaServer -> Float -> SpriteLocation -> Bool -> IO ()
        rotateWorkspaceHorizontally gss radians _ True = do
          Plugin.Types.rotateWorkspaceHorizontally gss radians Workspace
          return ()
        rotateWorkspaceHorizontally gss radians _ False = do
          return ()

        rotateWorkspacesHorizontally :: GodotSimulaServer -> Float -> SpriteLocation -> Bool -> IO ()
        rotateWorkspacesHorizontally gss radians _ True = do
          Plugin.Types.rotateWorkspaceHorizontally gss radians Workspaces
          return ()
        rotateWorkspacesHorizontally gss radians _ False = do
          return ()

        terminateWindow :: SpriteLocation -> Bool -> IO ()
        terminateWindow (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          case eitherSurface of
            (Left wlrXdgSurface) -> do
              wlrXdgSurface <- validateSurfaceE wlrXdgSurface
              toplevel  <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
              G.send_close toplevel
            (Right wlrXWaylandSurface) -> do
              wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
              G.send_close wlrXWaylandSurface
        terminateWindow _ _ = return ()
  
        shellLaunch :: GodotSimulaServer -> String -> SpriteLocation -> Bool -> IO ()
        shellLaunch gss shellCmd _ True = do
          appLaunch gss shellCmd Nothing
          return ()
        shellLaunch _ _ _ _ = return ()

        textToSpeech :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        textToSpeech gss _ True = do
          let originalEnv = (gss ^. gssOriginalEnv)
          maybeXwaylandDisplay <- readTVarIO (gss ^. gssXWaylandDisplay)
          case maybeXwaylandDisplay of
            Nothing -> putStrLn "No DISPLAY found!"
            (Just xwaylandDisplay) -> do
              let envMap = M.fromList originalEnv
              let envMapWithDisplay = M.insert "DISPLAY" xwaylandDisplay envMap
              let envListWithDisplay = M.toList envMapWithDisplay

              (_, output', _) <- B.readCreateProcessWithExitCode ((shell "./result/bin/xsel -p") { env = Just envListWithDisplay, new_session = True }) ""
              let output = (B.unpack output')
              createProcess (proc "./result/bin/mimic" ["-pw", "--setf", "duration_stretch=0.68", "-t", output]) { env = Just envListWithDisplay, new_session = True } :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
              return ()
        textToSpeech _ _ _ = return ()
  
        launchXpra' :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        launchXpra' gss _ True = do
          -- gss <- readTVarIO (gsvs ^. gsvsServer)
          launchXpra gss
        launchXpra' _ _ _ = return ()
  
        toggleGrabMode' :: SpriteLocation -> Bool -> IO ()
        toggleGrabMode' (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          toggleGrabMode
        toggleGrabMode' _ _ = return ()
  
        launchHMDWebCam' :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        launchHMDWebCam' gss _ True = do
          -- gss <- readTVarIO (gsvs ^. gsvsServer)
          launchHMDWebCam gss Nothing
          return ()
        launchHMDWebCam' _ _ _ = return ()
  
        orientWindowTowardsGaze :: SpriteLocation -> Bool -> IO ()
        orientWindowTowardsGaze (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
         orientSpriteTowardsGaze gsvs
        orientWindowTowardsGaze _ _ = return ()
  
        pushWindow :: SpriteLocation -> Bool -> IO ()
        pushWindow (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          moveSpriteAlongObjectZAxis gsvs 0.1
        pushWindow _ _ = return ()
  
        pullWindow :: SpriteLocation -> Bool -> IO ()
        pullWindow (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          moveSpriteAlongObjectZAxis gsvs (-0.1)
        pullWindow _ _ = return ()
  
        scaleWindowDown :: SpriteLocation -> Bool -> IO ()
        scaleWindowDown (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          V3 1 1 1 ^* (1 + 1 * (-0.1)) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)
        scaleWindowDown _ _ = return ()
  
        scaleWindowUp :: SpriteLocation -> Bool -> IO ()
        scaleWindowUp (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          V3 1 1 1 ^* (1 + 1 * (0.1)) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)
        scaleWindowUp _ _ = return ()
  
        zoomOut :: SpriteLocation -> Bool -> IO ()
        zoomOut (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          resizeGSVS gsvs Zoom 1.05
        zoomOut _ _ = return ()

        zoomIn :: SpriteLocation -> Bool -> IO ()
        zoomIn (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          resizeGSVS gsvs Zoom 0.95
        zoomIn _ _ = return ()

        horizontalContract :: SpriteLocation -> Bool -> IO ()
        horizontalContract (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          resizeGSVS gsvs Horizontal 0.95
        horizontalContract _ _ = return ()

        horizontalExtend :: SpriteLocation -> Bool -> IO ()
        horizontalExtend (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          resizeGSVS gsvs Horizontal 1.05
        horizontalExtend _ _ = return ()

        decreaseTransparency :: SpriteLocation -> Bool -> IO ()
        decreaseTransparency (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          transOld <- readTVarIO (gsvs ^. gsvsTransparency)
          let transNew = (constrainTransparency (transOld - 0.05))
          atomically $ writeTVar (gsvs ^. gsvsTransparency) transNew
          case (transOld == 1, transNew == 1) of
            (True, False) -> setShader gsvs "res://addons/godot-haskell-plugin/TextShader.tres"
            (False, True)-> setShader gsvs "res://addons/godot-haskell-plugin/TextShaderOpaque.tres"
            _ -> return ()
        decreaseTransparency _ _ = return ()

        increaseTransparency :: SpriteLocation -> Bool -> IO ()
        increaseTransparency (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          transOld <- readTVarIO (gsvs ^. gsvsTransparency)
          let transNew = (constrainTransparency (transOld + 0.05))
          atomically $ writeTVar (gsvs ^. gsvsTransparency) transNew
          case (transOld == 1, transNew == 1) of
            (True, False) -> setShader gsvs "res://addons/godot-haskell-plugin/TextShader.tres"
            (False, True)-> setShader gsvs "res://addons/godot-haskell-plugin/TextShaderOpaque.tres"
            _ -> return ()
        increaseTransparency _ _ = return ()

        toggleScreenshotMode :: SpriteLocation -> Bool -> IO ()
        toggleScreenshotMode (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          screenshotMode <- readTVarIO (gsvs ^. gsvsScreenshotMode)
          case screenshotMode of
            True -> do atomically $ writeTVar (gsvs ^. gsvsScreenshotMode) False
                       atomically $ writeTVar (gsvs ^. gsvsScreenshotCoords) (Nothing, Nothing)

            False -> do atomically $ writeTVar (gsvs ^. gsvsScreenshotMode) True
        toggleScreenshotMode _ _ = return ()

        takeScreenshotGlobal :: SpriteLocation -> Bool -> IO ()
        takeScreenshotGlobal _ True = do
          timeStampStr <- show <$> getCurrentTime
          let screenshotBaseName' = "simula_pancake_picture_" <> timeStampStr
          let screenshotBaseName = filter (\x -> x /= ' ') screenshotBaseName'
          logStr $ "Taking global pancake screenshot"
          screenshotFullPath <- savePngPancake gss screenshotBaseName
          return ()
        takeScreenshotGlobal _ _ = return ()

        verticalContract :: SpriteLocation -> Bool -> IO ()
        verticalContract (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          resizeGSVS gsvs Vertical 0.95
        verticalContract _ _ = return ()

        verticalExtend :: SpriteLocation -> Bool -> IO ()
        verticalExtend (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          resizeGSVS gsvs Vertical 1.05
        verticalExtend _ _ = return ()

        resizeWindowToDefaultSize :: SpriteLocation -> Bool -> IO ()
        resizeWindowToDefaultSize (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          -- orientSpriteTowardsGaze gsvs
          defaultSizeGSVS gsvs
        resizeWindowToDefaultSize _ _ = return ()

        reloadConfig :: SpriteLocation -> Bool -> IO ()
        reloadConfig _ True = do
          putStrLn "Reloading Simula config.."
          configuration <- parseConfiguration
          atomically $ writeTVar (gss ^. gssConfiguration) configuration
          let keyboardShortcutsVal = getKeyboardShortcuts gss (configuration ^. keyBindings)
          atomically $ writeTVar (gss ^. gssKeyboardShortcuts) keyboardShortcutsVal
          let keyboardRemappingsVal = getKeyboardRemappings gss (configuration ^. keyRemappings)
          atomically $ writeTVar (gss ^. gssKeyboardRemappings) keyboardRemappingsVal
          worldEnv@(worldEnvironment, _) <- readTVarIO (gss ^. gssWorldEnvironment)
          textures <- loadEnvironmentTextures configuration worldEnvironment
          atomically $ writeTVar (gss ^. gssEnvironmentTextures) textures
        reloadConfig _ _ = return ()

        terminateSimula :: SpriteLocation -> Bool -> IO ()
        terminateSimula _ True = do
          putStrLn "Terminating Simula.."
          pid <- getProcessID
          putStrLn $ "Terminating Simula with pid: " ++ (show pid)
          createProcess (shell $ "kill " ++ (show pid))
          return ()
        terminateSimula _ _ = return ()

        cycleEnvironment :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        cycleEnvironment gss _ True = do
          putStrLn "Cycling environment.."
          cycleGSSEnvironment gss
          return ()
        cycleEnvironment _ _ _ = do
          return ()

        toggleARMode :: GodotSimulaServer -> SpriteLocation -> Bool -> IO ()
        toggleARMode gss _ True = do
          putStrLn "Toggling AR mode.."
          (worldEnvironment, _) <- readTVarIO (gss ^. gssWorldEnvironment)
          environment <- G.get_environment worldEnvironment
          G.set_background environment G.BG_CAMERA_FEED
          G.set_camera_feed_id environment 1
          cameraServer <- getSingleton GodotCameraServer "CameraServer"
          numCameras <- G.get_feed_count cameraServer
          putStrLn $ "CameraServer get_feed_count: " ++ (show numCameras)
          when (numCameras > 0) $ G.get_feed cameraServer 0 >>= \cam -> G.set_active cam True
          return ()
        toggleARMode _ _ _ = do
          return ()

        debugPrint :: SpriteLocation -> Bool -> IO ()
        debugPrint _ True = do
          putStrLn "debugPrint"
        debugPrint _ _ = return ()

        switchToWorkspace :: GodotSimulaServer -> Int -> SpriteLocation -> Bool -> IO ()
        switchToWorkspace gss workspaceNum _ True = do
          putStrLn $ "Switching to workspace" ++ (show workspaceNum)
          currentWorkspace <- readTVarIO (gss ^. gssWorkspace)
          let newWorkspace = (gss ^. gssWorkspaces) V.! workspaceNum

          -- Set new workspace for new gsvs to inherit from
          atomically $ writeTVar (gss ^. gssWorkspace) newWorkspace

          -- Swap workspace in scene graph
          removeChild gss currentWorkspace
          addChild gss newWorkspace
          (canvasLayer, label) <- readTVarIO (gss ^. gssHUD)
          G.set_text label =<< (toLowLevel (pack (show workspaceNum)))
        switchToWorkspace _ _ _ _ = return ()

        sendToWorkspace :: GodotSimulaServer -> Int -> SpriteLocation -> Bool -> IO ()
        sendToWorkspace gss workspaceNum (Just (gsvs, coords@(SurfaceLocalCoordinates (sx, sy)))) True = do
          putStrLn $ "Sending app to workspace" ++ (show workspaceNum)
          currentWorkspace <- readTVarIO (gss ^. gssWorkspace)
          let newWorkspace = (gss ^. gssWorkspaces) V.! workspaceNum
          removeChild currentWorkspace gsvs
          addChild newWorkspace gsvs
        sendToWorkspace _ _ _ _ = return ()

isMask :: Int -> Bool
isMask keyOrMask = elem keyOrMask [ G.KEY_MASK_SHIFT
                                  , G.KEY_MASK_ALT
                                  , G.KEY_MASK_META
                                  , G.KEY_MASK_CTRL
                                  , G.KEY_MASK_CMD
                                  , G.KEY_MASK_KPAD
                                  , G.KEY_MASK_GROUP_SWITCH ]

separateModifiersFromKeycodes :: [Int] -> ([Modifiers], [Keycode])
separateModifiersFromKeycodes allKeys = let
  modifiers = filter isMask allKeys
  keys = filter (not . isMask) allKeys
  in (modifiers, keys)

getModifiersAndKeycodes :: [String] -> Maybe ([Modifiers], [Keycode])
getModifiersAndKeycodes keyboardShortcutLst = let
  scancodes = Control.Monad.Extra.sequence $ fmap getScancode keyboardShortcutLst :: Maybe [Int]
  in case scancodes of
        Nothing -> Nothing
        (Just scancodes') -> Just (separateModifiersFromKeycodes scancodes') :: Maybe ([Modifiers], [Keycode])

getKeyboardShortcutsEntries :: GodotSimulaServer -> KeyboardShortcut -> Maybe ((Modifiers, Keycode), KeyboardAction)
getKeyboardShortcutsEntries gss keyboardShortcut@(KeyboardShortcut keyCombination keyAction) = let
  modifiersAndKeycodes = getModifiersAndKeycodes keyCombination :: Maybe ([Modifiers], [Keycode])
  keyboardAction = getKeyboardAction gss keyboardShortcut

  in case  modifiersAndKeycodes of
     Just (modifiers, keycodes) -> Just (((foldl (.|.) 0 modifiers), (foldl (.|.) 0 keycodes)), keyboardAction)
     Nothing -> Nothing

getKeyboardShortcuts :: GodotSimulaServer -> [KeyboardShortcut] -> KeyboardShortcuts
getKeyboardShortcuts gss lstOfKeyboardShortcuts = let
  maybeEntries = fmap (getKeyboardShortcutsEntries gss) lstOfKeyboardShortcuts :: [Maybe ((Modifiers, Keycode), KeyboardAction)]
  entries = catMaybes maybeEntries :: [((Modifiers, Keycode), KeyboardAction)]
  entriesMap = M.fromList entries :: M.Map (Modifiers, Keycode) KeyboardAction
  in entriesMap

getKeyboardRemappings :: GodotSimulaServer -> [KeyboardRemapping] -> KeyboardRemappings
getKeyboardRemappings gss lstOfKeyboardRemapping = let
  maybeEntries = fmap (getTuple) lstOfKeyboardRemapping :: [Maybe (Scancode, Scancode)]
  entries = catMaybes maybeEntries :: [(Scancode, Scancode)]
  entriesMap = M.fromList entries :: M.Map Scancode Scancode
  in entriesMap
  where getTuple :: KeyboardRemapping -> Maybe (Scancode, Scancode)
        getTuple keyboardRemapping = let
          a = getScancode (keyboardRemapping ^. keyOriginal) :: Maybe Scancode
          b = getScancode (keyboardRemapping ^. keyMappedTo) :: Maybe Scancode
          in case (a, b) of
              (Just s1, Just s2) -> Just (s1, s2)
              _                  -> Nothing

instance NativeScript GodotSimulaServer where
  -- className = "SimulaServer"
  classInit spatial = initGodotSimulaServer (safeCast spatial)

  -- classExtends = "Spatial"
  classMethods =
    [ func NoRPC "_ready" (catchGodot Plugin.SimulaServer.ready)
    , func NoRPC "_input" (catchGodot Plugin.SimulaServer._input)
    , func NoRPC "_on_WaylandDisplay_ready"    (catchGodot Plugin.SimulaServer._on_WaylandDisplay_ready)
    , func NoRPC "_on_WlrXdgShell_new_surface" (catchGodot Plugin.SimulaServer._on_WlrXdgShell_new_surface)
    , func NoRPC "_on_wlr_key" (catchGodot Plugin.SimulaServer._on_wlr_key)
    , func NoRPC "_on_wlr_modifiers" (catchGodot Plugin.SimulaServer._on_wlr_modifiers)
    , func NoRPC "_on_WlrXWayland_new_surface" (catchGodot Plugin.SimulaServer._on_WlrXWayland_new_surface)
    , func NoRPC "_physics_process" (catchGodot Plugin.SimulaServer.physicsProcess)
    , func NoRPC "_on_simula_shortcut" (catchGodot Plugin.SimulaServer._on_simula_shortcut)
    , func NoRPC "handle_wlr_compositor_new_surface" (catchGodot Plugin.SimulaServer.handle_wlr_compositor_new_surface)
    , func NoRPC "seat_request_cursor" (catchGodot Plugin.SimulaServer.seat_request_cursor)
    ]

  classSignals = []

ready :: GodotSimulaServer -> [GodotVariant] -> IO ()
ready gss _ = do
  debugModeMaybe <- lookupEnv "DEBUG"
  rrModeMaybe <- lookupEnv "RUNNING_UNDER_RR"
  -- Delete log file
  case rrModeMaybe of
    Just "1" -> return ()
    _ -> do readProcess "touch" ["./log.txt"] []
            readProcess "rm" ["./log.txt"] []
            return ()

  addWlrChildren gss

  gssWorkspaces' <- V.replicateM 10 (unsafeInstance GodotSpatial "Spatial")
  gssDiffMapVal <- initializeDiffMap (safeCast gss) gssWorkspaces'
  atomically $ writeTVar (gss ^. gssDiffMap) gssDiffMapVal

  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  wlrKeyboard <- readTVarIO (gss ^. gssWlrKeyboard)

  G.set_keyboard wlrSeat (safeCast wlrKeyboard)

  -- Connect signals
  connectGodotSignal wlrKeyboard "key" gss "_on_wlr_key" []
  connectGodotSignal wlrKeyboard "modifiers" gss "_on_wlr_modifiers" []
  connectGodotSignal wlrKeyboard "shortcut" gss "_on_simula_shortcut" []
  connectGodotSignal wlrSeat "request_set_cursor" gss "seat_request_cursor" []
    -- Omission: We omit connecting "size_changed" with "_on_viewport_change"

  wlrCompositor <- readTVarIO (gss ^. gssWlrCompositor)
  wlrXWayland <- readTVarIO (gss ^. gssWlrXWayland)

  oldDisplay <- getEnv "DISPLAY"

  -- We wait till here to start XWayland so we can feed it a seat + compositor
  G.start_xwayland wlrXWayland (safeCast wlrCompositor) (safeCast wlrSeat)

  newDisplay <- getEnv "DISPLAY"
  putStr "New DISPLAY="
  putStrLn newDisplay
  setEnv "DISPLAY" oldDisplay
  if (newDisplay /= oldDisplay)
    then atomically $ writeTVar (gss ^. gssXWaylandDisplay) (Just newDisplay)
    else atomically $ writeTVar (gss ^. gssXWaylandDisplay) Nothing

  connectGodotSignal wlrXWayland "new_surface" gss "_on_WlrXWayland_new_surface" []

  startTelemetry (gss ^. gssViews)

  viewport <- G.get_viewport gss :: IO GodotViewport
  connectGodotSignal viewport "input_event" gss "_mouse_input" []

  getSingleton GodotInput "Input" >>= \inp -> G.set_mouse_mode inp G.MOUSE_MODE_CAPTURED
  pid <- getProcessID

  createProcess (shell "./result/bin/xrdb -merge .Xdefaults") { env = Just [("DISPLAY", newDisplay)] }

  case rrModeMaybe of
    Just "1" -> return ()
    _ -> do (_, windows', _) <- B.readCreateProcessWithExitCode (shell "./result/bin/wmctrl -lp") ""
            let windows = (B.unpack windows')
            let rightWindows = filter (\line -> isInfixOf (show pid) line) (lines windows)
            let simulaWindow = (head . words . head) rightWindows
            createProcess ((shell $ "./result/bin/wmctrl -ia " ++ simulaWindow) { env = Just [("DISPLAY", oldDisplay)] })
            return ()

  appendFile "log.txt" ""

  -- Adding a `WorldEnvironment` anywhere to an active scene graph
  -- overrides the default environment
  (worldEnvironment, _) <- readTVarIO (gss ^. gssWorldEnvironment)
  addChild gss worldEnvironment

  defaultWorkspace <- readTVarIO (gss ^. gssWorkspace)
  addChild gss defaultWorkspace

  -- Launch default apps
  sApps <- readTVarIO (gss ^. gssStartingApps)
  launchDefaultApps sApps "center"
  putStrLn $ "Launching default apps: " ++ (show sApps)

  case debugModeMaybe of
    Nothing -> return ()
    Just debugModeVal  -> (forkIO $ debugFunc gss) >> return ()

  (canvasLayer , label) <- readTVarIO (gss ^. gssHUD)
  addChild gss canvasLayer
  addChild canvasLayer label

  where launchDefaultApps :: [String] -> String-> IO ()
        launchDefaultApps sApps location = do
          let firstApp = if (sApps == []) then Nothing else Just (head sApps)
          let tailApps = tail sApps
          pid <- case firstApp of
                      Nothing -> return $ fromInteger 0
                      Just app -> do pid <- appLaunch gss app (Just location)
                                     return pid
          case location of
            "center" -> do
              launchDefaultApps tailApps "right"
            "right" -> do
              launchDefaultApps tailApps "bottom"
            "bottom" -> do
              launchDefaultApps tailApps "left"
            "left" -> do
              launchDefaultApps tailApps "top"
            "top" -> do
              return ()

-- | Populate the GodotSimulaServer's TVar's with Wlr types; connect some Wlr methods
-- | to their signals. This implicitly starts the compositor.
addWlrChildren :: GodotSimulaServer -> IO ()
addWlrChildren gss = do
  -- putStrLn "addWlrChildren"
  -- Here we assume gss is already a node in our scene tree.

  -- WaylandDisplay
  waylandDisplay <- unsafeInstance GodotWaylandDisplay "WaylandDisplay"
  setWaylandSocket waylandDisplay "simula-0"
  atomically $ writeTVar (_gssWaylandDisplay gss) waylandDisplay
  connectGodotSignal waylandDisplay "ready" gss "_on_WaylandDisplay_ready" [] -- [connection signal="ready" from="WaylandDisplay" to="." method="_on_WaylandDisplay_ready"]
  G.set_name waylandDisplay =<< toLowLevel "WaylandDisplay"
  G.add_child gss ((safeCast waylandDisplay) :: GodotNode) True -- Triggers "ready" signal, calls "_on_WaylandDisplay_ready", and starts the compositor

  -- We omit having ViewportBounds children

  -- Children of WaylandDisplay
  wlrDataDeviceManager <- unsafeInstance GodotWlrDataDeviceManager "WlrDataDeviceManager"
  atomically $ writeTVar (_gssWlrDataDeviceManager gss) wlrDataDeviceManager
  G.set_name wlrDataDeviceManager =<< toLowLevel "WlrDataDeviceManager"
  G.add_child ((safeCast waylandDisplay) :: GodotNode) ((safeCast wlrDataDeviceManager) :: GodotNode) True

  wlrBackend <- unsafeInstance GodotWlrBackend "WlrBackend"
  atomically $ writeTVar (_gssWlrBackend gss) wlrBackend
  G.set_name wlrBackend =<< toLowLevel "WlrBackend"
  G.add_child waylandDisplay ((safeCast wlrBackend) :: GodotNode) True

  wlrXdgShell <- unsafeInstance GodotWlrXdgShell "WlrXdgShell"
  connectGodotSignal wlrXdgShell "new_surface" gss "_on_WlrXdgShell_new_surface" [] -- [connection signal="new_surface" from="WaylandDisplay/WlrXdgShell" to="." method="_on_WlrXdgShell_new_surface"]
  atomically $ writeTVar (_gssWlrXdgShell gss) wlrXdgShell
  G.set_name wlrXdgShell =<< toLowLevel "WlrXdgShell"
  G.add_child waylandDisplay ((safeCast wlrXdgShell) :: GodotNode) True

  wlrXWayland <- unsafeInstance GodotWlrXWayland "WlrXWayland"
  -- Don't start XWayland until `ready`
  -- connectGodotSignal wlrXWayland "new_surface" gss "_on_WlrXWayland_new_surface" [] -- [connection signal="new_surface" from="WaylandDisplay/WlrXWayland" to="." method="_on_WlrXWayland_new_surface"]
  atomically $ writeTVar (_gssWlrXWayland gss) wlrXWayland
  G.set_name wlrXWayland =<< toLowLevel "WlrXWayland"
  G.add_child waylandDisplay ((safeCast wlrXWayland) :: GodotNode) True

  wlrSeat <- unsafeInstance GodotWlrSeat "WlrSeat"
  G.set_capabilities wlrSeat 3
  atomically $ writeTVar (_gssWlrSeat gss) wlrSeat
  G.set_name wlrSeat =<< toLowLevel "WlrSeat"
  G.add_child waylandDisplay ((safeCast wlrSeat) :: GodotNode) True

  wlrKeyboard <- unsafeInstance GodotWlrKeyboard "WlrKeyboard"
  atomically $ writeTVar (_gssWlrKeyboard gss) wlrKeyboard
  G.set_name wlrKeyboard =<< toLowLevel "WlrKeyboard"
  G.add_child waylandDisplay ((safeCast wlrKeyboard) :: GodotNode) True

  -- Children of WlrBackend
  wlrOutput <- unsafeInstance GodotWlrOutput "WlrOutput"
  atomically $ writeTVar (_gssWlrOutput gss) wlrOutput
  G.set_name wlrOutput =<< toLowLevel "WlrOutput"
  G.add_child wlrBackend ((safeCast wlrOutput) :: GodotNode) True

  wlrCompositor <- unsafeInstance GodotWlrCompositor "WlrCompositor"
  atomically $ writeTVar (_gssWlrCompositor gss) wlrCompositor
  G.set_name wlrCompositor =<< toLowLevel "WlrCompositor"
  G.add_child wlrBackend ((safeCast wlrCompositor) :: GodotNode) True
  connectGodotSignal wlrCompositor "new_surface" gss "handle_wlr_compositor_new_surface" []

  rc <- readTVarIO (gss ^. gssHMDRayCast)
  addChild gss rc

  return ()
  where setWaylandSocket :: GodotWaylandDisplay -> String -> IO ()
        setWaylandSocket waylandDisplay socketName = do
          socketName' <- toLowLevel (pack socketName)
          G.set_socket_name waylandDisplay socketName'

parseConfiguration :: IO (Configuration)
parseConfiguration = do
  profile <- lookupEnv "PROFILE"
  let defaultConfiguration = Configuration {_backend = "OpenVR", _startingApps = StartingApps {_center = Just "ENV=val ./result/bin/xfce4-terminal", _right = Nothing, _bottom = Nothing, _left = Nothing, _top = Nothing}, _defaultWindowResolution = Just (900,900), _defaultWindowScale = 1.0, _keyBindings = [KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_BACKSPACE"], _keyAction = "terminateWindow"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_ESCAPE"], _keyAction = "toggleGrabMode"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_SLASH"], _keyAction = "launchTerminal"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_APOSTROPHE"], _keyAction = "moveCursor"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_ENTER"], _keyAction = "clickLeft"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_ALT_R"], _keyAction = "grabWindow"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_ALT_L"], _keyAction = "grabWindow"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_A"], _keyAction = "launchAppLauncher"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_E"], _keyAction = "cycleEnvironment"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_F"], _keyAction = "orientWindowTowardsGaze"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_9"], _keyAction = "scaleWindowDown"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_0"], _keyAction = "scaleWindowUp"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_MINUS"], _keyAction = "zoomOut"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_EQUAL"], _keyAction = "zoomIn"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_LEFT"], _keyAction = "contractWindowHorizontally"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_RIGHT"], _keyAction = "extendWindowHorizontally"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_UP"], _keyAction = "contractWindowVertically"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_DOWN"], _keyAction = "extendWindowVertically"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_S"], _keyAction = "resizeWindowToDefaultSize"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_COMMA"], _keyAction = "pullWindow"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_PERIOD"], _keyAction = "pushWindow"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_W"], _keyAction = "launchHMDWebCam"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_R"], _keyAction = "emacsclient -c"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_MASK_SHIFT","KEY_ESCAPE"], _keyAction = "terminateSimula"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_MASK_ALT","KEY_UP"], _keyAction = "increaseTransparency"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_MASK_ALT","KEY_DOWN"], _keyAction = "decreaseTransparency"},KeyboardShortcut {_keyCombination = ["KEY_PRINT"], _keyAction = "toggleScreenshotMode"},KeyboardShortcut {_keyCombination = ["KEY_MASK_SHIFT","KEY_PRINT"], _keyAction = "takeScreenshotGlobal"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_K"], _keyAction = "firefox -new-window"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_G"], _keyAction = "google-chrome-stable --new-window news.ycombinator.com"},KeyboardShortcut {_keyCombination = ["KEY_MASK_META","KEY_J"], _keyAction = "gvim"}], _keyRemappings = [], _environmentsDirectory = "./environments", _environmentDefault = "./environments/AllSkyFree_Sky_EpicBlueSunset_Equirect.png"} --
  config <- case profile of
    Just value -> return defaultConfiguration
    Nothing -> input auto "./config/config.dhall" :: IO Configuration
  return config

-- | We first fill the TVars with dummy state, before updating them with their
-- | real values in `ready`.
initGodotSimulaServer :: GodotObject -> IO (GodotSimulaServer)
initGodotSimulaServer obj = do
  -- putStrLn "initGodotSimulaServer"
  gssWaylandDisplay'       <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWaylandDisplay)
  gssWlrBackend'           <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrBackend)
  gssWlrOutput'            <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrOutput)
  gssWlrCompositor'        <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrCompositor)
  gssWlrXdgShell'          <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrXdgShell)
  gssWlrSeat'              <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrSeat)
  gssWlrXWayland'          <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrXWayland)
  gssWlrDataDeviceManager' <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrDataDeviceManager)
  gssWlrKeyboard'          <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrKeyboard)
  gssViews'                <- newTVarIO M.empty                                          :: IO (TVar (M.Map SimulaView GodotSimulaViewSprite))
  gssKeyboardFocusedSprite' <- newTVarIO Nothing :: IO (TVar (Maybe GodotSimulaViewSprite))
  visualServer <- getSingleton GodotVisualServer "VisualServer"
  visualServer' <- newTVarIO visualServer
  gssActiveCursorGSVS' <- newTVarIO Nothing

  maybeCursorTexture <- getTextureFromURL "res://cursor.png"
  gssCursorTexture' <- newTVarIO maybeCursorTexture
  maybeScreenshotCursorTexture <- getTextureFromURL "res://cursor_plus.png"
  gssScreenshotCursorTexture' <- newTVarIO maybeScreenshotCursorTexture

  rc <- unsafeInstance GodotRayCast "RayCast"
  G.set_cast_to rc =<< toLowLevel (V3 0 0 (negate 10))
  G.set_enabled rc True
  gssHMDRayCast' <- newTVarIO rc

  gssKeyboardGrabbedSprite' <- newTVarIO Nothing
  gssXWaylandDisplay'       <- newTVarIO Nothing

  gssOriginalEnv' <- getEnvironment

  gssFreeChildren' <- newTVarIO M.empty :: IO (TVar (M.Map GodotWlrXWaylandSurface GodotSimulaViewSprite))

  rec
      configuration <- parseConfiguration
      gssConfiguration'       <- newTVarIO configuration :: IO (TVar Configuration)
      let keyboardShortcutsVal = getKeyboardShortcuts gss (configuration ^. keyBindings)
      gssKeyboardShortcuts'    <- newTVarIO keyboardShortcutsVal :: IO (TVar KeyboardShortcuts)
      let keyboardRemappingsVal = getKeyboardRemappings gss (configuration ^. keyRemappings)
      gssKeyboardRemappings'    <- newTVarIO keyboardRemappingsVal :: IO (TVar KeyboardRemappings)

      let sApps = getStartingAppsList (configuration ^. startingApps)
      let numberOfStartingApps = length sApps
      gssStartingApps' <- newTVarIO sApps

      panoramaSky      <- unsafeInstance GodotPanoramaSky "PanoramaSky"
      environment      <- unsafeInstance GodotEnvironment "Environment"
      worldEnvironment <- unsafeInstance GodotWorldEnvironment "WorldEnvironment"

      -- Environment
      G.set_background environment G.ENV_BG_SKY -- 2
      G.set_sky environment (safeCast panoramaSky)
      backgroundColor <- (toLowLevel $ (rgb 0.0 0.538333 0.703125) `withOpacity` 1) :: IO GodotColor
      G.set_bg_color environment backgroundColor
      ambientLightColor <- (toLowLevel $ (rgb 0.328125 0.328125 0.328125) `withOpacity` 1) :: IO GodotColor
      G.set_ambient_light_color environment ambientLightColor
      G.set_ssao_blur environment G.ENV_SSAO_BLUR_1x1 -- 1

      -- WorldEnvironment
      G.set_environment worldEnvironment environment
      -- G.set_transform gssWorldEnvironment Transform( 0.623013, -0.733525, 0.271654, 0.321394, 0.55667, 0.766044, -0.713134, -0.389948, 0.582563, 0, 100, 0 )

      let texStr = (configuration ^. environmentDefault)
      maybeDefaultTexture <- getTextureFromURL ("res://" ++ texStr)
      case maybeDefaultTexture of
           Nothing -> do putStrLn "Can't set panorama texture!"
           Just tex -> do G.set_panorama panoramaSky tex
      gssWorldEnvironment' <- newTVarIO (worldEnvironment, texStr)
      texturesStr <- loadEnvironmentTextures configuration worldEnvironment
      gssEnvironmentTextures' <- newTVarIO texturesStr
      gssStartingAppTransform' <- newTVarIO Nothing

      pid <- getProcessID
      let gssPid' = show pid

      gssStartingAppPids' <- newTVarIO M.empty :: IO (TVar (M.Map ProcessID [String]))

      gssGrab' <- newTVarIO Nothing

      gssWorkspaces' <- V.replicateM 10 (unsafeInstance GodotSpatial "Spatial")
      gssWorkspace' <- newTVarIO $ (gssWorkspaces' V.! 1)
      gssDiffMap' <- newTVarIO $ M.empty -- Can't instantiate this until we have the gss Spatial information

      canvasLayer <- unsafeInstance GodotCanvasLayer "CanvasLayer"
      -- offset <- toLowLevel (V2 10 10) :: IO GodotVector2 -- Doesn't seem to work
      -- G.set_offset canvasLayer offset
      -- G.set_custom_viewport viewportNode -- To be used for affecting the VR viewport
      scale <- toLowLevel (V2 2 2) :: IO GodotVector2
      G.set_scale canvasLayer scale

      label <- unsafeInstance GodotLabel "Label"
      G.set_text label =<< (toLowLevel "1")

      -- G.set_valign label G.VALIGN_CENTER -- VALIGN_TOP, VALIGN_CENTER, VALIGN_BOTTOM, VALI layer value
      -- G.set_visible_characters label <int> -- -1 to  layer value
      -- G.set_align layer value

      gssHUD' <- newTVarIO (canvasLayer, label) :: IO (TVar HUD)

      let gss = GodotSimulaServer {
        _gssObj                   = obj                       :: GodotObject
      , _gssWaylandDisplay        = gssWaylandDisplay'        :: TVar GodotWaylandDisplay
      , _gssWlrBackend            = gssWlrBackend'            :: TVar GodotWlrBackend
      , _gssWlrOutput             = gssWlrOutput'             :: TVar GodotWlrOutput
      , _gssWlrCompositor         = gssWlrCompositor'         :: TVar GodotWlrCompositor
      , _gssWlrXdgShell           = gssWlrXdgShell'           :: TVar GodotWlrXdgShell
      , _gssWlrXWayland           = gssWlrXWayland'           :: TVar GodotWlrXWayland
      , _gssWlrSeat               = gssWlrSeat'               :: TVar GodotWlrSeat
      , _gssWlrDataDeviceManager  = gssWlrDataDeviceManager'  :: TVar GodotWlrDataDeviceManager
      , _gssWlrKeyboard           = gssWlrKeyboard'           :: TVar GodotWlrKeyboard
      , _gssViews                 = gssViews'                 :: TVar (M.Map SimulaView GodotSimulaViewSprite)
      , _gssKeyboardFocusedSprite = gssKeyboardFocusedSprite' :: TVar (Maybe GodotSimulaViewSprite)
      , _gssVisualServer          = visualServer'             :: TVar GodotVisualServer
      , _gssActiveCursorGSVS      = gssActiveCursorGSVS'      :: TVar (Maybe GodotSimulaViewSprite)
      , _gssCursorTexture         = gssCursorTexture'         :: TVar (Maybe GodotTexture)
      , _gssScreenshotCursorTexture = gssScreenshotCursorTexture' :: TVar (Maybe GodotTexture)
      , _gssHMDRayCast            = gssHMDRayCast'            :: TVar GodotRayCast
      , _gssKeyboardGrabbedSprite = gssKeyboardGrabbedSprite' :: TVar (Maybe (GodotSimulaViewSprite, Float))
      , _gssXWaylandDisplay       = gssXWaylandDisplay'       :: TVar (Maybe String)
      , _gssOriginalEnv           = gssOriginalEnv'           :: [(String, String)]
      , _gssFreeChildren          = gssFreeChildren'          :: TVar (M.Map GodotWlrXWaylandSurface GodotSimulaViewSprite)
      , _gssConfiguration         = gssConfiguration'         :: TVar Configuration
      , _gssKeyboardShortcuts     = gssKeyboardShortcuts'     :: TVar KeyboardShortcuts
      , _gssKeyboardRemappings    = gssKeyboardRemappings'    :: TVar KeyboardRemappings
      , _gssStartingApps          = gssStartingApps'          :: TVar [String]
      , _gssWorldEnvironment      = gssWorldEnvironment'      :: TVar (GodotWorldEnvironment, String)
      , _gssEnvironmentTextures   = gssEnvironmentTextures'   :: TVar [String]
      , _gssStartingAppTransform  = gssStartingAppTransform'  :: TVar (Maybe GodotTransform)
      , _gssPid                   = gssPid'                   :: String
      , _gssStartingAppPids       = gssStartingAppPids'       :: TVar (M.Map ProcessID [String])
      , _gssGrab                  = gssGrab'                  :: TVar (Maybe Grab)
      , _gssDiffMap               = gssDiffMap'               :: TVar (M.Map GodotSpatial GodotTransform)
      , _gssWorkspaces            = gssWorkspaces'            :: Vector GodotSpatial
      , _gssWorkspace             = gssWorkspace'             :: TVar GodotSpatial
      , _gssHUD                   = gssHUD'                   :: TVar HUD
      }
  return gss
  where getStartingAppsStr :: Maybe String -> String
        getStartingAppsStr Nothing = "nullApp"
        getStartingAppsStr (Just str) = str

        getStartingAppsList :: StartingApps -> [String]
        getStartingAppsList startingApps =
            fmap getStartingAppsStr [(startingApps ^. center), (startingApps ^. right), (startingApps ^. bottom), (startingApps ^. left), (startingApps ^. top)]

_on_WaylandDisplay_ready :: GodotSimulaServer -> [GodotVariant] -> IO ()
_on_WaylandDisplay_ready gss _ = do
  waylandDisplay <- atomically $ readTVar (_gssWaylandDisplay gss)
  G.run waylandDisplay
  return ()

_on_WlrXdgShell_new_surface :: GodotSimulaServer -> [GodotVariant] -> IO ()
_on_WlrXdgShell_new_surface gss [wlrXdgSurfaceVariant] = do
  putStrLn "_on_WlrXdgShell_new_surface"
  wlrXdgSurface <- (fromGodotVariant wlrXdgSurfaceVariant :: IO GodotWlrXdgSurface) >>= validateSurfaceE
  roleInt <- G.get_role wlrXdgSurface
  case roleInt of
      0 -> return () -- XDG_SURFACE_ROLE_NONE
      2 -> do -- XDG_SURFACE_ROLE_POPUP
        wlrSurface <- G.get_wlr_surface wlrXdgSurface >>= validateSurfaceE
        maybeGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
        case maybeGSVS of
          Nothing -> putStrLn "Unable to connect xdg popup surface signals; no gssActiveCursorGSVS!"
          Just gsvs -> do
            -- connectGodotSignal wlrSurface "new_subsurface" gsvs "handle_wlr_surface_new_subsurface" [] -- arguably don't need; subsumed by xdg new_popup signal
            -- connectGodotSignal wlrSurface "commit" gsvs "handle_wlr_surface_commit" []
            -- connectGodotSignal wlrSurface "destroy" gsvs "handle_wlr_surface_destroy" []  -- arguably don't need; subsumed by xdg destroy signal
            return ()
      1 -> do -- XDG_SURFACE_ROLE_TOPLEVEL
              wlrXdgToplevel <- G.get_xdg_toplevel wlrXdgSurface >>= validateSurfaceE
              wlrSurface <- G.get_wlr_surface wlrXdgSurface >>= validateSurfaceE
              G.set_tiled wlrXdgToplevel True
              simulaView <- newSimulaView gss wlrXdgSurface
              gsvs <- newGodotSimulaViewSprite gss simulaView

              connectGodotSignal gsvs "map" gss "handle_map_surface" []
              connectGodotSignal wlrXdgSurface "destroy" gsvs "_handle_destroy" []
              connectGodotSignal wlrXdgSurface "map" gsvs "_handle_map" []
              connectGodotSignal wlrXdgSurface "unmap" gsvs "handle_unmap" []
              connectGodotSignal wlrXdgSurface "new_popup" gsvs "handle_new_popup" []
              connectGodotSignal wlrXdgToplevel "request_show_window_menu" gsvs "handle_window_menu" []
              connectGodotSignal wlrSurface "new_subsurface" gsvs "handle_wlr_surface_new_subsurface" []
              connectGodotSignal wlrSurface "commit" gsvs "handle_wlr_surface_commit" []
              connectGodotSignal wlrSurface "destroy" gsvs "handle_wlr_surface_destroy" []

              -- We G.set_activated early to prevent weird behavior (e.g. file pickers failing to spawn)
              G.set_activated wlrXdgToplevel True
              return ()


   where newSimulaView :: GodotSimulaServer -> GodotWlrXdgSurface -> IO (SimulaView)
         newSimulaView gss wlrXdgSurface = do
          let gss' = gss :: GodotSimulaServer
          svMapped' <- atomically (newTVar False) :: IO (TVar Bool)
          let gsvsWlrXdgSurface' = wlrXdgSurface
          gsvsUUID' <- nextUUID :: IO (Maybe UUID)

          return SimulaView
              { _svServer           = gss :: GodotSimulaServer
              , _svMapped           = svMapped' :: TVar Bool
              , _svWlrEitherSurface = (Left wlrXdgSurface) :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface
              , _gsvsUUID           = gsvsUUID' :: Maybe UUID
              }

_on_wlr_key :: GodotSimulaServer -> [GodotVariant] -> IO ()
_on_wlr_key gss [keyboardGVar, eventGVar] = do
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  event <- fromGodotVariant eventGVar
  G.reference event
  G.keyboard_notify_key wlrSeat event
  return ()

_on_wlr_modifiers :: GodotSimulaServer -> [GodotVariant] -> IO ()
_on_wlr_modifiers gss [keyboardGVar] = do
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  G.keyboard_notify_modifiers wlrSeat
  return ()

_on_WlrXWayland_new_surface :: GodotSimulaServer -> [GodotVariant] -> IO ()
_on_WlrXWayland_new_surface gss [wlrXWaylandSurfaceVariant] = do
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  G.reference wlrXWaylandSurface
  simulaView <- newSimulaView gss wlrXWaylandSurface
  gsvs <- newGodotSimulaViewSprite gss simulaView

  connectGodotSignal gsvs "map" gss "handle_map_surface" []
  connectGodotSignal wlrXWaylandSurface "map_free_child" gsvs "handle_map_free_child" []
  connectGodotSignal wlrXWaylandSurface "map_child" gsvs "handle_map_child" []
  connectGodotSignal wlrXWaylandSurface "destroy" gsvs "_handle_destroy" []
  connectGodotSignal wlrXWaylandSurface "map" gsvs "_handle_map" []
  connectGodotSignal wlrXWaylandSurface "unmap" gsvs "handle_unmap" []
  connectGodotSignal wlrXWaylandSurface "unmap_child" gsvs "handle_unmap_child" []
  connectGodotSignal wlrXWaylandSurface "unmap_free_child" gsvs "handle_unmap_free_child" []
  connectGodotSignal wlrXWaylandSurface "set_parent" gsvs "handle_set_parent" []
  return ()
  where newSimulaView :: GodotSimulaServer -> GodotWlrXWaylandSurface -> IO (SimulaView)
        newSimulaView gss wlrXWaylandSurface = do
         let gss' = gss :: GodotSimulaServer
         svMapped' <- atomically (newTVar False) :: IO (TVar Bool)
         -- let gsvsWlrXWaylandSurface' = wlrXWaylandSurface
         gsvsUUID' <- nextUUID :: IO (Maybe UUID)

         return SimulaView
             { _svServer           = gss :: GodotSimulaServer
             , _svMapped           = svMapped' :: TVar Bool
             , _svWlrEitherSurface = (Right wlrXWaylandSurface) :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface
             , _gsvsUUID           = gsvsUUID' :: Maybe UUID
             }

-- Find the cursor-active gsvs, convert relative godot mouse movement to new
-- mouse coordinates, and pass off to processClickEvent or pointer_notify_axis
_input :: GodotSimulaServer -> [GodotVariant] -> IO ()
_input gss [eventGV] = do
  event <- fromGodotVariant eventGV :: IO GodotInputEventMouseMotion
  maybeActiveGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)

  whenM (event `isClass` "InputEventMouseMotion") $ do
     mouseRelativeGV2 <- G.get_relative event :: IO GodotVector2
     mouseRelative@(V2 dx dy) <- fromLowLevel mouseRelativeGV2
     case maybeActiveGSVS of
         Nothing -> return ()
         (Just gsvs) -> do updateCursorStateRelative gsvs dx dy
                           sendWlrootsMotion gsvs
  whenM (event `isClass` "InputEventMouseButton") $ do
    let event' = GodotInputEventMouseButton (coerce event)
    pressed <- G.is_pressed event'
    button <- G.get_button_index event'
    wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
    case (maybeActiveGSVS, button) of
         (Just gsvs, G.BUTTON_WHEEL_UP) -> G.pointer_notify_axis_continuous wlrSeat 0 (0.05)
         (Just gsvs, G.BUTTON_WHEEL_DOWN) -> G.pointer_notify_axis_continuous wlrSeat 0 (-0.05)
         (Just gsvs, _) -> do
           screenshotMode <- readTVarIO (gsvs ^. gsvsScreenshotMode)
           case screenshotMode of
             False -> do activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
                         processClickEvent' gsvs (Button pressed button) activeGSVSCursorPos
             True -> do activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
                        case pressed of
                          True -> do atomically $ writeTVar (gsvs ^. gsvsScreenshotCoords) (Just activeGSVSCursorPos, Nothing)
                          False -> do screenshotCoords@(origin, end) <- readTVarIO (gsvs ^. gsvsScreenshotCoords)
                                      atomically $ writeTVar (gsvs ^. gsvsScreenshotCoords) (origin, Just activeGSVSCursorPos)
         (Nothing, _) -> return ()

updateCursorStateRelative :: GodotSimulaViewSprite -> Float -> Float -> IO ()
updateCursorStateRelative gsvs dx dy = do
    activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
    cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
    textureViewport <- readTVarIO (cb ^. cbViewport)
    tvGV2 <- G.get_size textureViewport
    (V2 mx my) <- fromLowLevel tvGV2

    let sx' = if ((sx + dx) < mx) then (sx + dx) else mx
    let sx'' = if (sx' > 0) then sx' else 0
    let sy' = if ((sy + dy) < my) then (sy + dy) else my
    let sy'' = if (sy' > 0) then sy' else 0
    atomically $ writeTVar (gsvs ^. gsvsCursorCoordinates) (SurfaceLocalCoordinates (sx'', sy''))

updateCursorStateAbsolute :: GodotSimulaViewSprite -> Float -> Float -> IO ()
updateCursorStateAbsolute gsvs sx sy = do
    cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
    textureViewport <- readTVarIO (cb ^. cbViewport)
    tvGV2 <- G.get_size textureViewport
    (V2 mx my) <- fromLowLevel tvGV2

    let sx' = if (sx < mx) then sx else mx
    let sx'' = if (sx' > 0) then sx' else 0
    let sy' = if (sy < my) then sy else my
    let sy'' = if (sy' > 0) then sy' else 0
    atomically $ writeTVar (gsvs ^. gsvsCursorCoordinates) (SurfaceLocalCoordinates (sx'', sy''))

sendWlrootsMotion :: GodotSimulaViewSprite -> IO ()
sendWlrootsMotion gsvs = do
    screenshotModeEnabled <- readTVarIO (gsvs ^. gsvsScreenshotMode)
    activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
    case screenshotModeEnabled of
      False -> processClickEvent' gsvs Motion activeGSVSCursorPos
      True -> return ()

getHMDLookAtSprite :: GodotSimulaServer -> IO (Maybe (GodotSimulaViewSprite, SurfaceLocalCoordinates))
getHMDLookAtSprite gss = do
  rc <- readTVarIO (gss ^.  gssHMDRayCast)
  G.force_raycast_update rc -- Necessary to avoid crashes
  hmdGlobalTransform <- getARVRCameraOrPancakeCameraTransformGlobal gss
  G.set_global_transform rc hmdGlobalTransform

  isColliding <- G.is_colliding rc
  maybeSprite <- if isColliding then G.get_collider rc >>= asNativeScript :: IO (Maybe GodotSimulaViewSprite) else (return Nothing)
  ret <- case maybeSprite of
            Nothing -> return Nothing
            Just gsvs -> do gv3 <- G.get_collision_point rc :: IO GodotVector3
                            surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) <- getSurfaceLocalCoordinates gsvs gv3
                            return $ Just (gsvs, surfaceLocalCoords)
  return ret


physicsProcess :: GodotSimulaServer -> [GodotVariant] -> IO ()
physicsProcess gss _ = do
  maybeLookAtGSVS <- getHMDLookAtSprite gss
  gsvsActiveCursor <- readTVarIO (gss ^. gssActiveCursorGSVS)
  maybeGssGrab <- readTVarIO (gss ^. gssGrab)
  case maybeGssGrab of
    (Just (GrabWindows prevPovTransform)) -> do currentWorkspace <- readTVarIO (gss ^. gssWorkspace)
                                                diffMap <- readTVarIO (gss ^. gssDiffMap)
                                                diff <- getGrabDiff gss
                                                id <- makeIdentityTransform
                                                let diffPrev = M.findWithDefault id currentWorkspace diffMap
                                                diffComposed <- Api.godot_transform_operator_multiply diff diffPrev
                                                G.set_transform currentWorkspace diffComposed
    (Just (GrabWindow gsvs dist)) -> do setInFrontOfUser gsvs dist
                                        orientSpriteTowardsGaze gsvs
    (Just (GrabWorkspaces prevPovTransform)) -> do currentWorkspace <- readTVarIO (gss ^. gssWorkspace)
                                                   diffMap <- readTVarIO (gss ^. gssDiffMap)
                                                   diff <- getGrabDiff gss
                                                   id <- makeIdentityTransform
                                                   let diffPrev = M.findWithDefault id (safeCast gss) diffMap
                                                   diffComposed <- Api.godot_transform_operator_multiply diff diffPrev
                                                   G.set_transform gss diffComposed
    Nothing -> case maybeLookAtGSVS of
                    Nothing -> return ()
                    Just (gsvs, surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy))) -> do
                        case gsvsActiveCursor of
                              Nothing -> focus gsvs
                              Just gsvsActiveCursor' -> if (gsvs /= gsvsActiveCursor') then (focus gsvs) else return () -- (safeSetActivated gsvs True)

-- Run shell command with DISPLAY set to its original (typically :1).
shellCmd1 :: GodotSimulaServer -> String -> IO ()
shellCmd1 gss appStr = do
  let originalEnv = (gss ^. gssOriginalEnv)
  createProcess (shell appStr) { env = Just originalEnv }
  return ()

_on_simula_shortcut :: GodotSimulaServer -> [GodotVariant] -> IO ()
_on_simula_shortcut gss [scancodeWithModifiers', isPressed'] = do
  scancodeWithModifiers <- fromGodotVariant scancodeWithModifiers' :: IO Int
  isPressed <- fromGodotVariant isPressed' :: IO Bool
  wlrKeyboard <- readTVarIO $ (gss ^. gssWlrKeyboard)
  keyboardShortcuts <- readTVarIO (gss ^. gssKeyboardShortcuts)
  keyboardRemappings <- readTVarIO (gss ^. gssKeyboardRemappings)
  maybeHMDLookAtSprite <- getHMDLookAtSprite gss
  let modifiers = (foldl (.|.) 0 (extractMods scancodeWithModifiers))
  let keycode = (scancodeWithModifiers .&. G.KEY_CODE_MASK) :: Int
  let maybeKeyboardAction = M.lookup (modifiers, keycode) keyboardShortcuts
  let maybeKeycodeRemapping = M.lookup keycode keyboardRemappings

  let keycode' = case maybeKeycodeRemapping of
        (Just remappedKeycode) -> remappedKeycode
        Nothing                -> keycode

  case maybeKeyboardAction of
    Just action -> action maybeHMDLookAtSprite isPressed
    Nothing -> case isPressed of
                 False -> do if (keycode' == keyNull) then return () else G.send_wlr_event_keyboard_key wlrKeyboard keycode' isPressed
                             keyboardGrabLetGo' gss maybeHMDLookAtSprite -- HACK: Avoid windows getting stuck when modifiers are disengaged before keys
                             keyboardGrabLetGo gss (GrabWindows undefined) -- HACK: Avoid windows getting stuck when modifiers are disengaged before keys
                 True -> if (keycode' == keyNull) then return () else G.send_wlr_event_keyboard_key wlrKeyboard keycode' isPressed

  where keyboardGrabLetGo' :: GodotSimulaServer -> Maybe (GodotSimulaViewSprite, SurfaceLocalCoordinates) -> IO ()
        keyboardGrabLetGo' gss (Just (gsvs, _)) = do keyboardGrabLetGo gss (GrabWindow gsvs undefined)
        keyboardGrabLetGo' _ _ = return ()

        extractMods :: Int -> [Int]
        extractMods sc = concatMap (extractIf sc) [G.KEY_MASK_SHIFT, G.KEY_MASK_ALT, G.KEY_MASK_META, G.KEY_MASK_CTRL, G.KEY_MASK_CMD, G.KEY_MASK_KPAD, G.KEY_MASK_GROUP_SWITCH]
        extractIf sc mod = if (sc .&. mod) /= 0 then [mod] else []

        keyNull = 0
extractMods _ = []

launchXpra :: GodotSimulaServer -> IO ()
launchXpra gss = do
  let originalEnv = (gss ^. gssOriginalEnv)
  maybeXwaylandDisplay <- readTVarIO (gss ^. gssXWaylandDisplay)
  case maybeXwaylandDisplay of
    Nothing -> putStrLn "No DISPLAY found!"
    (Just xwaylandDisplay) -> do
      let envMap = M.fromList originalEnv
      let envMapWithDisplay = M.insert "DISPLAY" xwaylandDisplay envMap
      let envListWithDisplay = M.toList envMapWithDisplay

      (_,output',_) <- B.readCreateProcessWithExitCode (shell "./result/bin/xpra list") ""
      let output = B.unpack output'
      let isXpraAlreadyLive = isInfixOf ":13" output
      case isXpraAlreadyLive of
        False -> do createSessionLeader "./result/bin/xpra" ["--fake-xinerama=no", "start", "--start", "./result/bin/xfce4-terminal", ":13"] (Just envListWithDisplay)
                    waitForXpraRecursively
        True -> do putStrLn "xpra is already running!"
      createSessionLeader "./result/bin/xpra" ["attach", ":13"] (Just envListWithDisplay)
      return ()
      where waitForXpraRecursively = do
              (_,output',_) <- B.readCreateProcessWithExitCode (shell "./result/bin/xpra list") ""
              let output = B.unpack output'
              putStrLn $ "Output is: " ++ output
              let isXpraAlreadyLive = isInfixOf ":13" output
              case isXpraAlreadyLive of
                False -> do putStrLn $ "Waiting for xpra server.."
                            waitForXpraRecursively
                True -> do putStrLn "xpra server found!"

createSessionLeader :: FilePath -> [String] -> Maybe [(String, String)] -> IO (ProcessID, ProcessGroupID)
createSessionLeader exe args env = do
  pid <- forkProcess $ do
    createSession
    executeFile exe True args env
  pgid <- getProcessGroupIDOf pid
  return (pid, pgid)

createProcessWithGroup :: ProcessGroupID -> FilePath -> [String] -> Maybe [(String, String)] -> IO ProcessID
createProcessWithGroup pgid exe args env =
   forkProcess $ do
    joinProcessGroup pgid
    executeFile exe True args env

-- | HACK: `G.set_mouse_mode` is set to toggle the grab on *both* the keyboard and
-- | the mouse cursor.
toggleGrabMode :: IO ()
toggleGrabMode = do
  getSingleton GodotInput "Input" >>= \inp -> do
    mode <- G.get_mouse_mode inp
    case mode of
      G.MOUSE_MODE_CAPTURED -> G.set_mouse_mode inp G.MOUSE_MODE_VISIBLE
      G.MOUSE_MODE_VISIBLE -> G.set_mouse_mode inp G.MOUSE_MODE_CAPTURED
  return ()

handle_wlr_compositor_new_surface :: GodotSimulaServer -> [GodotVariant] -> IO ()
handle_wlr_compositor_new_surface gss args@[wlrSurfaceVariant] = do
  putStrLn "handle_wlr_compositor_new_surface"
  wlrSurface <- (fromGodotVariant wlrSurfaceVariant :: IO GodotWlrSurface) >>= validateSurfaceE
  maybeGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  case maybeGSVS of
    Nothing -> return () -- putStrLn "Unable to handle_wlr_compositor_new_surface; no gssActiveCursorGSVS!"
    Just gsvs -> do
      connectGodotSignal wlrSurface "new_subsurface" gsvs "handle_wlr_surface_new_subsurface" []
      connectGodotSignal wlrSurface "commit" gsvs "handle_wlr_surface_commit" []
      connectGodotSignal wlrSurface "destroy" gsvs "handle_wlr_surface_destroy" []
      return ()

seat_request_cursor :: GodotSimulaServer -> [GodotVariant] -> IO ()
seat_request_cursor gss args@[wlrSurfaceCursorVariant] = do
  wlrSurfaceCursor <- (fromGodotVariant wlrSurfaceCursorVariant :: IO GodotWlrSurface) >>= validateSurfaceE
  maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  case maybeActiveCursorGSVS of
      Nothing -> putStrLn "Unable to find active cursor gsvs; unable to load cursor texture."
      Just gsvs -> atomically $ writeTVar (gsvs ^. gsvsCursor) ((Just wlrSurfaceCursor), Nothing)
  return ()