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

-- To (probably) be used with Simula's resizing function (to make sure we stop propogating wayland events during grabs).
-- data SimulaCursorMode = SimulaCursorPassthrough | SimulaCursorMove |  SimulaCursorResize

instance GodotClass GodotSimulaServer where
  godotClassName = "SimulaServer"

instance ClassExport GodotSimulaServer where
  classInit obj  = initGodotSimulaServer obj

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" Plugin.SimulaServer.ready
    , GodotMethod NoRPC "_input" Plugin.SimulaServer.input
    ]

  -- Test:
  classSignals = [ signal "test_signal1" [("arg1", GodotVariantTypeVector3), ("arg2", GodotVariantTypeObject)]
                 , signal "test_signal2" []
                 ]

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _) = GodotSpatial obj

ready :: GFunc GodotSimulaServer
ready gss _ = do
  -- startSimulaServerOnNewThread gss
  -- startTelemetry (gss ^. gssViews) -- Doesn't seem to require previous function to get to displayRunServer
  toLowLevel VariantNil

input :: GFunc GodotSimulaServer
input self args = do
  -- (getArg' 0 args :: IO GodotObject)
  --   >>= asClass GodotInputEventKey "InputEventKey" >>= \case
  --     Just evk -> do
  --       processKeyEvent self evk
  --       setInputHandled self

  --     Nothing  -> return () -- not a key
  toLowLevel VariantNil

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


initGodotSimulaServer :: GodotObject -> IO (GodotSimulaServer)
initGodotSimulaServer obj = mdo
  let gss = GodotSimulaServer {
      _gssObj           = undefined :: GodotObject
    , _gssDisplay       = undefined :: DisplayServer
    , _gssViews         = undefined :: TVar (M.Map SimulaView GodotSimulaViewSprite)
    , _gssBackend       = undefined :: Ptr Backend
    , _gssXdgShell      = undefined :: Ptr WlrXdgShell
    , _gssSeat          = undefined :: Ptr WlrSeat
    , _gssKeyboards     = undefined :: TVar [SimulaKeyboard]
    , _gssOutputs       = undefined :: TVar [SimulaOutput]
    , _gssRenderer      = undefined :: Ptr Renderer
    , _gssNewXdgSurface = undefined :: ListenerToken
  }
  return gss

-- | Here we set socket(s), start the backend, and finally call displayRun (which
-- | blocks until the compositor is shut down). Once the compositor is shut down,
-- | we free everything.
startSimulaServerOnNewThread :: GodotSimulaServer -> IO ()
startSimulaServerOnNewThread gss = void $ forkOS $ mdo
      -- prevDisplay <- getEnv "DISPLAY"
      setLogPrio Debug

      let socketName = "simula-0"
      putStrLn $ "Socket: " ++ socketName
      setEnv "WAYLAND_DISPLAY" socketName
      displayAddSocket (gss ^. gssDisplay) (Just socketName)

      -- This just calls backend_start, which with wlr_godot_backend is just a shim
      -- (compare to the backend_start of the headless backend).
      backendStart (gss ^. gssBackend)

      -- In case it matters for debugging: I'm assuming that WlRoots doesn't force
      -- us to change compositor frame internals like we had to with Weston, i.e.:
        -- $(struct weston_compositor * compositor')->repaint_msec = 1000
        -- forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000

      -- setEnv "DISPLAY" prevDisplay
      displayRun (gss ^. gssDisplay)
      -- destroyDisplayClients displayServer
      displayDestroy (gss ^. gssDisplay)
      freeListenerToken (gss ^. gssNewXdgSurface)
      -- freeListenerToken (gss ^. gssNewInput)  -- Not needed now that we're using wlr_godot_backend
      -- freeListenerToken (gss ^. gssNewOutput) -- "

      where destroyDisplayClients displayServer = do
              let displayServer' = toInlineC displayServer
              [C.exp| void { wl_display_destroy_clients($(struct wl_display * displayServer'))} |]

-- | This is a terrible hack. It requires a dummy GodotNode as its first
-- | argument (can possibly cast nullPtr as GodotNode internally?). Moreover, 
-- | it uses a hardcoded path that may change in the future. Finally, it somehow
-- | doesn't return a Maybe type (attempts to access a null Godot instance
-- | will just crash our program). See https://docs.godotengine.org/en/3.0/classes/class_node.html#class-node-get-node
getSimulaServerFromHardcodedNodePath :: GodotNode -> IO (GodotSimulaServer)
getSimulaServerFromHardcodedNodePath node = do
  gss <- getGodotSimualServerFromNodePath (node :: GodotNode) "/root/Root/Simula" -- Confusingly, this path is set in Simula.hs and not this module
  return gss
  where getGodotSimualServerFromNodePath :: GodotNode -> String -> IO GodotSimulaServer
        getGodotSimualServerFromNodePath node nodePathStr = do
          nodePath <- (toLowLevel (pack nodePathStr))
          gssNode <- G.get_node node nodePath
          -- G.print_tree ((safeCast gssNode) :: GodotNode)
          -- let gss = (unsafeCoerce gssNode) :: GodotSimulaServer
          gss <- (fromNativeScript (safeCast gssNode)) :: IO GodotSimulaServer
          return gss
