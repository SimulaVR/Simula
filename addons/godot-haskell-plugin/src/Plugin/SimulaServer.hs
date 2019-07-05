{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Plugin.SimulaServer where

import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript
import           Godot.Extra.Register

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
import           Data.Either

import           Foreign hiding (void)
import           Foreign.C.Error
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Text.XkbCommon.Keysym
import           Text.XkbCommon.KeyboardState
import           Text.XkbCommon.InternalTypes
import           Text.XkbCommon.Context
import           Text.XkbCommon.Keymap

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
import           Graphics.Wayland.WlRoots.Compositor
import           Graphics.Wayland.WlRoots.Output
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.WlRoots.Backend
import           Graphics.Wayland.WlRoots.Backend.Headless
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.Render
import           Graphics.Wayland.WlRoots.Render.Color
import           Graphics.Wayland.WlRoots.OutputLayout
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Seat
import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.XCursorManager
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Input.Keyboard
import           Graphics.Wayland.WlRoots.Input.Pointer
import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.Input.Buttons
import           Graphics.Wayland.WlRoots.Box
import           Graphics.Wayland.WlRoots.Util
import           Graphics.Wayland.WlRoots.DeviceManager

import           System.Clock
import           Control.Monad.Extra

initializeSimulaCtxAndIncludes

instance GodotClass GodotSimulaServer where
  godotClassName = "SimulaServer"

instance ClassExport GodotSimulaServer where
  classInit obj  = initGodotSimulaServer obj

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" Plugin.SimulaServer.ready
    -- , GodotMethod NoRPC "_input" Plugin.SimulaServer.input -- replaced by _on_wlr_* handlers
    , GodotMethod NoRPC "_on_WaylandDisplay_ready"    Plugin.SimulaServer._on_WaylandDisplay_ready
    , GodotMethod NoRPC "_on_WlrXdgShell_new_surface" Plugin.SimulaServer._on_WlrXdgShell_new_surface
    , GodotMethod NoRPC "handle_map_surface" Plugin.SimulaServer.handle_map_surface
    , GodotMethod NoRPC "handle_unmap_surface" Plugin.SimulaServer.handle_unmap_surface
    , GodotMethod NoRPC "_on_wlr_key" Plugin.SimulaServer._on_wlr_key
    , GodotMethod NoRPC "_on_wlr_modifiers" Plugin.SimulaServer._on_wlr_modifiers
    , GodotMethod NoRPC "_on_WlrXWayland_new_surface" Plugin.SimulaServer._on_WlrXWayland_new_surface
    ]

  classSignals = []

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _ _) = GodotSpatial obj

ready :: GFunc GodotSimulaServer
ready gss _ = do
  -- putStrLn "ready in SimulaServer.hs"
  -- Set state / start compositor
  addWlrChildren gss

  -- Get state
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  wlrKeyboard <- readTVarIO (gss ^. gssWlrKeyboard)
  wlrKeyboardGV <- asGodotVariant wlrKeyboard

  -- Set state
  G.set_keyboard wlrSeat wlrKeyboardGV

  -- Connect signals
  connectGodotSignal wlrKeyboard "key" gss "_on_wlr_key" []
  connectGodotSignal wlrKeyboard "modifiers" gss "_on_wlr_modifiers" []
    -- Omission: We omit connecting "size_changed" with "_on_viewport_change"

  -- wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  wlrSeatGV <- asGodotVariant wlrSeat
  wlrCompositor <- readTVarIO (gss ^. gssWlrCompositor)
  wlrCompositorGV <- asGodotVariant wlrCompositor
  wlrXWayland <- readTVarIO (gss ^. gssWlrXWayland)
  G.start_xwayland wlrXWayland wlrCompositorGV wlrSeatGV
  connectGodotSignal wlrXWayland "new_surface" gss "_on_WlrXWayland_newSurface" []

  -- Start telemetry
  startTelemetry (gss ^. gssViews)

  toLowLevel VariantNil

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
  G.add_child gss ((safeCast waylandDisplay) :: GodotObject) True -- Triggers "ready" signal, calls "_on_WaylandDisplay_ready", and starts the compositor

  -- We omit having ViewportBounds children

  -- Children of WaylandDisplay
  wlrDataDeviceManager <- unsafeInstance GodotWlrDataDeviceManager "WlrDataDeviceManager"
  atomically $ writeTVar (_gssWlrDataDeviceManager gss) wlrDataDeviceManager
  G.set_name wlrDataDeviceManager =<< toLowLevel "WlrDataDeviceManager"
  G.add_child waylandDisplay ((safeCast wlrDataDeviceManager) :: GodotObject) True

  wlrBackend <- unsafeInstance GodotWlrBackend "WlrBackend"
  atomically $ writeTVar (_gssWlrBackend gss) wlrBackend
  G.set_name wlrBackend =<< toLowLevel "WlrBackend"
  G.add_child waylandDisplay ((safeCast wlrBackend) :: GodotObject) True

  wlrXdgShell <- unsafeInstance GodotWlrXdgShell "WlrXdgShell"
  connectGodotSignal wlrXdgShell "new_surface" gss "_on_WlrXdgShell_new_surface" [] -- [connection signal="new_surface" from="WaylandDisplay/WlrXdgShell" to="." method="_on_WlrXdgShell_new_surface"]
  atomically $ writeTVar (_gssWlrXdgShell gss) wlrXdgShell
  G.set_name wlrXdgShell =<< toLowLevel "WlrXdgShell"
  G.add_child waylandDisplay ((safeCast wlrXdgShell) :: GodotObject) True

  wlrXWayland <- unsafeInstance GodotWlrXWayland "WlrXWayland"
  connectGodotSignal wlrXWayland "new_surface" gss "_on_WlrXWayland_new_surface" [] -- [connection signal="new_surface" from="WaylandDisplay/WlrXWayland" to="." method="_on_WlrXWayland_new_surface"]
  atomically $ writeTVar (_gssWlrXWayland gss) wlrXWayland
  G.set_name wlrXWayland =<< toLowLevel "WlrXWayland"
  G.add_child waylandDisplay ((safeCast wlrXWayland) :: GodotObject) True

  wlrSeat <- unsafeInstance GodotWlrSeat "WlrSeat"
  G.set_capabilities wlrSeat 3
  atomically $ writeTVar (_gssWlrSeat gss) wlrSeat
  G.set_name wlrSeat =<< toLowLevel "WlrSeat"
  G.add_child waylandDisplay ((safeCast wlrSeat) :: GodotObject) True

  wlrKeyboard <- unsafeInstance GodotWlrKeyboard "WlrKeyboard"
  atomically $ writeTVar (_gssWlrKeyboard gss) wlrKeyboard
  G.set_name wlrKeyboard =<< toLowLevel "WlrKeyboard"
  G.add_child waylandDisplay ((safeCast wlrKeyboard) :: GodotObject) True

  -- Children of WlrBackend
  wlrOutput <- unsafeInstance GodotWlrOutput "WlrOutput"
  atomically $ writeTVar (_gssWlrOutput gss) wlrOutput
  G.set_name wlrOutput =<< toLowLevel "WlrOutput"
  G.add_child wlrBackend ((safeCast wlrOutput) :: GodotObject) True

  wlrCompositor <- unsafeInstance GodotWlrCompositor "WlrCompositor"
  atomically $ writeTVar (_gssWlrCompositor gss) wlrCompositor
  G.set_name wlrCompositor =<< toLowLevel "WlrCompositor"
  G.add_child wlrBackend ((safeCast wlrCompositor) :: GodotObject) True

  return ()
  where setWaylandSocket :: GodotWaylandDisplay -> String -> IO ()
        setWaylandSocket waylandDisplay socketName = do
          socketName' <- toLowLevel (pack socketName)
          G.set_socket_name waylandDisplay socketName'

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

  let gss = GodotSimulaServer {
    _gssObj                  = obj                      :: GodotObject
  , _gssWaylandDisplay       = gssWaylandDisplay'       :: TVar GodotWaylandDisplay
  , _gssWlrBackend           = gssWlrBackend'           :: TVar GodotWlrBackend
  , _gssWlrOutput            = gssWlrOutput'            :: TVar GodotWlrOutput
  , _gssWlrCompositor        = gssWlrCompositor'        :: TVar GodotWlrCompositor
  , _gssWlrXdgShell          = gssWlrXdgShell'          :: TVar GodotWlrXdgShell
  , _gssWlrXWayland          = gssWlrXWayland'          :: TVar GodotWlrXWayland
  , _gssWlrSeat              = gssWlrSeat'              :: TVar GodotWlrSeat
  , _gssWlrDataDeviceManager = gssWlrDataDeviceManager' :: TVar GodotWlrDataDeviceManager
  , _gssWlrKeyboard          = gssWlrKeyboard'          :: TVar GodotWlrKeyboard
  , _gssViews                = gssViews'                :: TVar (M.Map SimulaView GodotSimulaViewSprite)
  }

  return gss

-- Don't think we should need this. Delete after a while.
-- getSimulaServerNodeFromPath :: GodotSimulaServer -> String -> IO a
-- getSimulaServerNodeFromPath gss nodePathStr = do
--   nodePath <- (toLowLevel (pack nodePathStr))
--   gssNode <- G.get_node ((safeCast gss) :: GodotNode) nodePath
--   ret  <- (fromNativeScript (safeCast gssNode)) :: IO a
--   return ret

_on_WaylandDisplay_ready :: GFunc GodotSimulaServer
_on_WaylandDisplay_ready gss vecOfGodotVariant = do
  -- putStrLn "_on_WaylandDisplay_ready"
  --waylandDisplay <- getSimulaServerNodeFromPath gss "WaylandDisplay"
  waylandDisplay <- atomically $ readTVar (_gssWaylandDisplay gss)
  G.run waylandDisplay
  toLowLevel VariantNil

_on_WlrXdgShell_new_surface :: GFunc GodotSimulaServer
_on_WlrXdgShell_new_surface gss args = do
  -- putStrLn "_on_WlrXdgShell_new_surface"
  case toList args of
    [wlrXdgSurfaceVariant] -> do
      wlrXdgSurface <- fromGodotVariant wlrXdgSurfaceVariant :: IO GodotWlrXdgSurface -- Not sure if godot-haskell provides this for us
      roleInt <- G.get_role wlrXdgSurface
      case roleInt of
          0 -> toLowLevel VariantNil -- XDG_SURFACE_ROLE_NONE
          2 -> toLowLevel VariantNil -- XDG_SURFACE_ROLE_POPUP
          1 -> do                    -- XDG_SURFACE_ROLE_TOPLEVEL
                  simulaView <- newSimulaView gss wlrXdgSurface
                  gsvs <- newGodotSimulaViewSprite gss simulaView

                  -- Mutate the server with our updated state
                  atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)

                  --surface.connect("map", self, "handle_map_surface")
                  connectGodotSignal gsvs "map" gss "handle_map_surface" []
                  --surface.connect("unmap", self, "handle_unmap_surface")
                  connectGodotSignal gsvs "unmap" gss "handle_unmap_surface" []

                  -- _xdg_surface_set logic from godotston:
                  -- xdg_surface.connect("destroy", self, "_handle_destroy"):
                  connectGodotSignal gsvs "destroy" gsvs "_handle_destroy" []
                  -- xdg_surface.connect("map", self, "_handle_map"):
                  connectGodotSignal gsvs "map" gsvs "_handle_map" []
                  -- xdg_surface.connect("unmap", self, "_handle_unmap"):
                  connectGodotSignal gsvs "unmap" gsvs "_handle_unmap" []

                  -- Add the gsvs as a child to the SimulaServer
                  G.add_child ((safeCast gss) :: GodotNode )
                              ((safeCast gsvs) :: GodotObject)
                              True

                  -- emitSignal gsvs "map" ([gsvs] :: [GodotSimulaViewSprite]) -- hack to ensure map functions get called; needs fixed

                  -- Handles 2D window movement across a viewport; not needed:
                  -- toplevel.connect("request_move", self, "_handle_request_move")
                  toLowLevel VariantNil

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

handle_map_surface :: GFunc GodotSimulaServer
handle_map_surface gss args = do
  -- putStrLn "handle_map_surface"
  case toList args of
    [gsvsVariant] -> do -- Unlike in Godotston, we assume this function gives us a GodotSimulaViewSprite
       maybeGsvs <- variantToReg gsvsVariant :: IO (Maybe GodotSimulaViewSprite)
       case maybeGsvs of
         Nothing -> putStrLn "Failed to cast GodotSimulaViewSprite!"
         Just gsvs -> do focus gsvs
                         simulaView <- atomically $ readTVar (gsvs ^. gsvsView)
                         atomically $ writeTVar (simulaView ^. svMapped) True
                         -- addChild gss gsvs -- Performed in _on_WlrXdgShell_new_surface
       
    _ -> putStrLn "Failed to get arguments in handle_map_surface" 
  toLowLevel VariantNil

handle_unmap_surface :: GFunc GodotSimulaServer
handle_unmap_surface gss args = do
  -- putStrLn "handle_unmap_surface"
  case toList args of
    [gsvsVariant] -> do -- Unlike in Godotston, we assume this function gives us a GodotSimulaViewSprite
       maybeGsvs <- variantToReg gsvsVariant :: IO (Maybe GodotSimulaViewSprite)
       case maybeGsvs of
         Nothing -> putStrLn "Failed to cast GodotSimulaViewSprite!"
         Just gsvs -> do simulaView <- atomically $ readTVar (gsvs ^. gsvsView)
                         atomically $ writeTVar (simulaView ^. svMapped) False
                         removeChild gss gsvs
                         -- Deletion should be handled elsewhere.
       toLowLevel VariantNil

_on_wlr_key :: GFunc GodotSimulaServer
_on_wlr_key gss args = do
  -- putStrLn "_on_wlr_key"
  case toList args of
    [keyboardGVar, eventGVar] -> do
      -- putStrLn "G.keyboard_notify_key"
      wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
      G.keyboard_notify_key wlrSeat eventGVar

      toLowLevel VariantNil

_on_wlr_modifiers :: GFunc GodotSimulaServer
_on_wlr_modifiers gss args = do
  -- putStrLn "_on_wlr_modifiers"
  case toList args of
    [keyboardGVar] -> do
      -- putStrLn "G.keyboard_notify_modifiers"
      wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
      G.keyboard_notify_modifiers wlrSeat

      toLowLevel VariantNil

  
_on_WlrXWayland_new_surface :: GFunc GodotSimulaServer
_on_WlrXWayland_new_surface gss args = do
  -- putStrLn "_on_WlrXdgShell_new_surface"
  case toList args of
    [wlrXWaylandSurfaceVariant] -> do
      wlrXWaylandSurface <- fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface
      putStrLn "_on_WlrXdgShell_new_surface not yet implemented!"
      retnil
  --     simulaView <- newSimulaView gss wlrXdgSurface
  --     gsvs <- newGodotSimulaViewSprite gss simulaView

  --     -- Mutate the server with our updated state
  --     atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)

  --     --surface.connect("map", self, "handle_map_surface")
  --     connectGodotSignal gsvs "map" gss "handle_map_surface" []
  --     --surface.connect("unmap", self, "handle_unmap_surface")
  --     connectGodotSignal gsvs "unmap" gss "handle_unmap_surface" []

  --     -- _xdg_surface_set logic from godotston:
  --     -- xdg_surface.connect("destroy", self, "_handle_destroy"):
  --     connectGodotSignal gsvs "destroy" gsvs "_handle_destroy" []
  --     -- xdg_surface.connect("map", self, "_handle_map"):
  --     connectGodotSignal gsvs "map" gsvs "_handle_map" []
  --     -- xdg_surface.connect("unmap", self, "_handle_unmap"):
  --     connectGodotSignal gsvs "unmap" gsvs "_handle_unmap" []

  --     -- Add the gsvs as a child to the SimulaServer
  --     G.add_child ((safeCast gss) :: GodotNode )
  --                 ((safeCast gsvs) :: GodotObject)
  --                 True

  --     -- emitSignal gsvs "map" ([gsvs] :: [GodotSimulaViewSprite]) -- hack to ensure map functions get called; needs fixed

  --     -- Handles 2D window movement across a viewport; not needed:
  --     -- toplevel.connect("request_move", self, "_handle_request_move")
  --     toLowLevel VariantNil

  -- where newSimulaView 
        --   :: GodotSimulaServer -> GodotWlrXWaylandSurface -> IO (SimulaView)
        -- newSimulaView gss wlrXWaylandSurface = do
        --  let gss' = gss :: GodotSimulaServer
        --  svMapped' <- atomically (newTVar False) :: IO (TVar Bool)
        --  let gsvsWlrXWaylandSurface' = wlrXdgSurface
        --  gsvsUUID' <- nextUUID :: IO (Maybe UUID)

        --  return SimulaView
        --      { _svServer           = gss :: GodotSimulaServer
        --      , _svMapped           = svMapped' :: TVar Bool
        --      , _svWlrEitherSurface = (Right wlrXWaylandSurface) :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface
        --      , _gsvsUUID           = gsvsUUID' :: Maybe UUID
        --      }