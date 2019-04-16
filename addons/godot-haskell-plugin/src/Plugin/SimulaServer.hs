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
    ]

  classSignals = []

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _) = GodotSpatial obj

ready :: GFunc GodotSimulaServer
ready gss _ = do
  -- Set state / start compositor
  addWlrChildren gss

  -- Get state
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  wlrKeyboard <- readTVarIO (gss ^. gssWlrKeyboard)
  wlrKeyboardGV <- asGodotVariant wlrKeyboard

  -- Set state
  G.set_keyboard wlrSeat wlrKeyboardGV

  -- Connect signals
  connectGodotSignal gss "key" gss "_on_wlr_key" []
  connectGodotSignal gss "modifiers" gss "_on_wlr_modifiers" []
    -- Omission: We omit connecting "size_changed" with "_on_viewport_change"

  -- Start telemetry
  startTelemetry (gss ^. gssViews)

  toLowLevel VariantNil

-- | Populate the GodotSimulaServer's TVar's with Wlr types; connect some Wlr methods
-- | to their signals. This implicitly starts the compositor.
addWlrChildren :: GodotSimulaServer -> IO ()
addWlrChildren gss = do
  -- Here we assume gss is already a node in our scene tree.
  waylandDisplay <- unsafeInstance GodotWaylandDisplay "WaylandDisplay"
  G.set_name waylandDisplay =<< toLowLevel "WaylandDisplay"
  G.add_child gss ((safeCast waylandDisplay) :: GodotObject) True
  -- We omit having ViewportBounds children

  -- Children of WaylandDisplay
  wlrDataDeviceManager <- unsafeInstance GodotWlrDataDeviceManager "WlrDataDeviceManager"
  G.set_name wlrDataDeviceManager =<< toLowLevel "WlrDataDeviceManager"
  G.add_child waylandDisplay ((safeCast wlrDataDeviceManager) :: GodotObject) True

  wlrBackend <- unsafeInstance GodotWlrBackend "WlrBackend"
  G.set_name wlrBackend =<< toLowLevel "WlrBackend"
  G.add_child waylandDisplay ((safeCast wlrBackend) :: GodotObject) True

  wlrXdgShell <- unsafeInstance GodotWlrXdgShell "WlrXdgShell"
  G.set_name wlrXdgShell =<< toLowLevel "WlrXdgShell"
  G.add_child waylandDisplay ((safeCast wlrXdgShell) :: GodotObject) True

  wlrSeat <- unsafeInstance GodotWlrSeat "WlrSeat"
  G.set_name wlrSeat =<< toLowLevel "WlrSeat"
  G.add_child waylandDisplay ((safeCast wlrSeat) :: GodotObject) True

  wlrKeyboard <- unsafeInstance GodotWlrKeyboard "WlrKeyboard"
  G.set_name wlrKeyboard =<< toLowLevel "WlrKeyboard"
  G.add_child waylandDisplay ((safeCast wlrKeyboard) :: GodotObject) True

  -- Children of WlrBackend
  wlrOutput <- unsafeInstance GodotWlrOutput "WlrOutput"
  G.set_name wlrOutput =<< toLowLevel "WlrOutput"
  G.add_child wlrBackend ((safeCast wlrOutput) :: GodotObject) True

  wlrCompositor <- unsafeInstance GodotWlrCompositor "WlrCompositor"
  G.set_name wlrCompositor =<< toLowLevel "WlrCompositor"
  G.add_child wlrBackend ((safeCast wlrCompositor) :: GodotObject) True

  -- Update GodotSimulaServer's TVars
  atomically $ writeTVar (_gssWaylandDisplay gss) waylandDisplay
  atomically $ writeTVar (_gssWlrBackend gss) wlrBackend
  atomically $ writeTVar (_gssWlrOutput gss) wlrOutput
  atomically $ writeTVar (_gssWlrCompositor gss) wlrCompositor
  atomically $ writeTVar (_gssWlrXdgShell gss) wlrXdgShell
  atomically $ writeTVar (_gssWlrSeat gss) wlrSeat
  atomically $ writeTVar (_gssWlrDataDeviceManager gss) wlrDataDeviceManager
  atomically $ writeTVar (_gssWlrKeyboard gss) wlrKeyboard
  
  -- Connect signals from Wlr types to their methods

  -- [connection signal="ready" from="WaylandDisplay" to="." method="_on_WaylandDisplay_ready"]
  connectGodotSignal waylandDisplay "ready" gss "_on_WaylandDisplay_ready" []
  -- [connection signal="new_surface" from="WaylandDisplay/WlrXdgShell" to="." method="_on_WlrXdgShell_new_surface"]
  connectGodotSignal wlrXdgShell "new_surface" gss "_on_WaylandDisplay_new_surface" []
  return ()

-- TODO: check the origin plane?
moveToUnoccupied :: GodotSimulaServer -> GodotSimulaViewSprite -> IO ()
moveToUnoccupied gss gsvs = do
  viewMap <- atomically $ readTVar (_gssViews gss)
  let otherGsvs = filter (\x -> asObj x /= asObj gsvs) $ M.elems viewMap

  extents <- forM otherGsvs $ \viewSprite -> do
    sprite <- atomically $ readTVar (gsvs ^. gsvsSprite) -- getSprite viewSprite
    aabb   <- G.get_transformed_aabb sprite
    size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
    pos    <- Api.godot_aabb_get_position aabb >>= fromLowLevel
    return (pos, size + pos)

  let minX = minimum $ 0 : map (view $ _1._x) extents
      maxX = maximum $ 0 :  map (view $ _2._x) extents
  sprite <- atomically $ readTVar (gsvs ^. gsvsSprite)
  aabb   <- G.get_aabb sprite
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
      newPos =
        if abs minX < abs maxX
        then V3 (minX - sizeX/2) 0 0
        else V3 (maxX + sizeX/2) 0 0

  G.translate gsvs =<< toLowLevel newPos


-- | We first fill the TVars with dummy state, before updating them with their
-- | real values in `ready`.
initGodotSimulaServer :: GodotObject -> IO (GodotSimulaServer)
initGodotSimulaServer obj = do
  gssWaylandDisplay'       <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWaylandDisplay)
  gssWlrBackend'           <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrBackend)
  gssWlrOutput'            <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrOutput)
  gssWlrCompositor'        <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrCompositor)
  gssWlrXdgShell'          <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrXdgShell)
  gssWlrSeat'              <- newTVarIO (error "Failed to initialize GodotSimulaServer") :: IO (TVar GodotWlrSeat)
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
  --waylandDisplay <- getSimulaServerNodeFromPath gss "WaylandDisplay"
  waylandDisplay <- atomically $ readTVar (_gssWaylandDisplay gss)
  G.run waylandDisplay
  toLowLevel VariantNil

_on_WlrXdgShell_new_surface :: GFunc GodotSimulaServer
_on_WlrXdgShell_new_surface gss args = do
  case toList args of
    [wlrXdgSurfaceVariant] -> do
      wlrXdgSurface <- fromGodotVariant wlrXdgSurfaceVariant :: IO GodotWlrXdgSurface -- Not sure if godot-haskell provides this for us
      -- enum XdgSurfaceRole {
      -- 	XDG_SURFACE_ROLE_NONE,     -- = 0
      -- 	XDG_SURFACE_ROLE_TOPLEVEL, -- = 1
      -- 	XDG_SURFACE_ROLE_POPUP,    -- = 2
      -- };
      roleInt <- G.get_role wlrXdgSurface
      case roleInt of
          0 -> toLowLevel VariantNil -- XDG_SURFACE_ROLE_NONE
          2 -> toLowLevel VariantNil -- XDG_SURFACE_ROLE_POPUP
          1 -> do                    -- XDG_SURFACE_ROLE_TOPLEVEL
                  simulaView <- newSimulaView gss wlrXdgSurface
                  gsvs <- newGodotSimulaViewSprite gss simulaView

                  -- Mutate the server with our updated state
                  atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)

                  -- Add the gsvs as a child to the SimulaServer
                  G.add_child ((safeCast gss) :: GodotNode )
                              ((safeCast gsvs) :: GodotObject)
                              True

                  --surface.connect("map", self, "handle_map_surface")
                  connectGodotSignal gss "map" gss "handle_map_surface" []
                  --surface.connect("unmap", self, "handle_unmap_surface")
                  connectGodotSignal gss "unmap" gss "handle_unmap_surface" []

                  toLowLevel VariantNil

   where newSimulaView :: GodotSimulaServer -> GodotWlrXdgSurface -> IO (SimulaView)
         newSimulaView gss wlrXdgSurface = do
          let gss' = gss :: GodotSimulaServer
          svMapped' <- atomically (newTVar False) :: IO (TVar Bool)
          let gsvsWlrXdgSurface' = wlrXdgSurface
          gsvsUUID' <- nextUUID :: IO (Maybe UUID)

          return SimulaView
              { _svServer          = gss :: GodotSimulaServer
              , _svMapped          = svMapped' :: TVar Bool
              , _gsvsWlrXdgSurface = wlrXdgSurface :: GodotWlrXdgSurface 
              , _gsvsUUID          = gsvsUUID' :: Maybe UUID
              }

handle_map_surface :: GFunc GodotSimulaServer
handle_map_surface gss args = do
  case toList args of
    [gsvsVariant] -> do -- Unlike in Godotston, we assume this function gives us a GodotSimulaViewSprite
       maybeGsvs <- variantToReg gsvsVariant :: IO (Maybe GodotSimulaViewSprite)
       case maybeGsvs of
         Nothing -> putStrLn "Failed to cast GodotSimulaViewSprite!"
         Just gsvs -> do focus gsvs
                         simulaView <- atomically $ readTVar (gsvs ^. gsvsView)
                         atomically $ writeTVar (simulaView ^. svMapped) True
                         -- addChild gss gsvs -- Performed in _on_WlrXdgShell_new_surface
       toLowLevel VariantNil

handle_unmap_surface :: GFunc GodotSimulaServer
handle_unmap_surface gss args = do
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
  case toList args of
    [keyboardGVar, eventGVar] -> do
      wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
      G.keyboard_notify_key wlrSeat eventGVar

      toLowLevel VariantNil

_on_wlr_modifiers :: GFunc GodotSimulaServer
_on_wlr_modifiers gss args = do
  case toList args of
    [keyboardGVar] -> do
      wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
      G.keyboard_notify_modifiers wlrSeat

      toLowLevel VariantNil