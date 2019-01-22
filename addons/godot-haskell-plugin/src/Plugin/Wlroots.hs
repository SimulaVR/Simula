{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Wlroots (GodotWlrootsCompositor(..)) where

import Foreign

import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript
import           Godot.Extra.Register

import qualified Data.Map.Strict as M

import Plugin.Input
import Plugin.WlrootsSurfaceSprite
import Plugin.WlrootsSurfaceTexture

import Control.Monad
import Control.Concurrent
import System.Environment

import System.Process

import Control.Lens

import Foreign hiding (void)

import Telemetry

import qualified Language.C.Inline as C
import Debug.C

import      Graphics.Wayland.WlRoots.Compositor
import      Graphics.Wayland.WlRoots.Output
import      Graphics.Wayland.WlRoots.Surface
import      Graphics.Wayland.WlRoots.Backend
import      Graphics.Wayland.WlRoots.Output
import      Graphics.Wayland.Server
-- import      System.InputDevice

{- HsRoots Modules:
import      Graphics.Egl
import      Graphics.Pixman
import      Graphics.Wayland.Global
import      Graphics.Wayland.List
import      Graphics.Wayland.Resource
import      Graphics.Wayland.Server.Client
import      Graphics.Wayland.Signal
import      Graphics.Wayland.WlRoots.Backend
import      Graphics.Wayland.WlRoots.Backend.Headless
import      Graphics.Wayland.WlRoots.Backend.Libinput
import      Graphics.Wayland.WlRoots.Backend.Multi
import      Graphics.Wayland.WlRoots.Backend.Session
import      Graphics.Wayland.WlRoots.Box
import      Graphics.Wayland.WlRoots.Buffer
import      Graphics.Wayland.WlRoots.Compositor
import      Graphics.Wayland.WlRoots.Cursor
import      Graphics.Wayland.WlRoots.DeviceManager
import      Graphics.Wayland.WlRoots.ExportDMABuf
import      Graphics.Wayland.WlRoots.Egl
import      Graphics.Wayland.WlRoots.GammaControl
import      Graphics.Wayland.WlRoots.Global
import      Graphics.Wayland.WlRoots.IdleInhibit
import      Graphics.Wayland.WlRoots.Input
import      Graphics.Wayland.WlRoots.Input.Buttons
import      Graphics.Wayland.WlRoots.Input.Keyboard
import      Graphics.Wayland.WlRoots.Input.Pointer
import      Graphics.Wayland.WlRoots.Input.Tablet
import      Graphics.Wayland.WlRoots.Input.TabletPad
import      Graphics.Wayland.WlRoots.Input.TabletTool
import      Graphics.Wayland.WlRoots.Input.Touch
import      Graphics.Wayland.WlRoots.InputInhibitor
import      Graphics.Wayland.WlRoots.LinuxDMABuf
import      Graphics.Wayland.WlRoots.Output
import      Graphics.Wayland.WlRoots.OutputLayout
import      Graphics.Wayland.WlRoots.PrimarySelection
import      Graphics.Wayland.WlRoots.Render
import      Graphics.Wayland.WlRoots.Render.Color
import      Graphics.Wayland.WlRoots.Render.Gles2
import      Graphics.Wayland.WlRoots.Render.Matrix
import      Graphics.Wayland.WlRoots.Screenshooter
import      Graphics.Wayland.WlRoots.Seat
import      Graphics.Wayland.WlRoots.ServerDecoration
import      Graphics.Wayland.WlRoots.Surface
import      Graphics.Wayland.WlRoots.SurfaceLayers
import      Graphics.Wayland.WlRoots.Tabletv2
import      Graphics.Wayland.WlRoots.Util
import      Graphics.Wayland.WlRoots.Util.Region
import      Graphics.Wayland.WlRoots.WlShell
import      Graphics.Wayland.WlRoots.XCursor
import      Graphics.Wayland.WlRoots.XCursorManager
import      Graphics.Wayland.WlRoots.XWayland
import      Graphics.Wayland.WlRoots.XdgShell
import      Graphics.Wayland.WlRoots.XdgShellv6
-} 

-- C.verbatim "#define WLR_USE_UNSTABLE"
-- C.include "<wayland-server.h>"
-- C.include "<wlr/backend.h>"
-- C.include "<wlr/render/wlr_renderer.h>" -- Throws error "expected primary-expression before 'static'  on line 37
initializeSimulaCtxAndIncludes

-- TODO: Rename typename to "Compositor" and propogate throughout godot scene tree.
data GodotWlrootsCompositor = GodotWlrootsCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar (Ptr WlrCompositor)
  , _gwcWlDisplay :: TVar DisplayServer
  , _gwcWlEventLoop :: TVar EventLoop
  , _gwcBackEnd :: TVar (Ptr Backend)
  , _gwcSurfaces :: TVar (M.Map (Ptr WlrSurface) GodotWlrootsSurfaceSprite)
  , _gwcOutput :: TVar (Ptr WlrOutput) -- possibly needs to be (TVar [Ptr WlrOutput])
--   , _gwcNormalLayer :: TVar (Ptr C'WlrLayer) -- so far unused
  }

{- inline-C datatype representation
data GodotWlrootsCompositor = GodotWlrootsCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar (Ptr C'WlrCompositor)
  , _gwcWlDisplay :: TVar C'WlDisplay  -- DisplayServer ~ * wl_display
  , _gwcWlEventLoop :: TVar C'WlEventLoop
  , _gwcBackEnd :: TVar (Ptr C'WlrBackend)
  , _gwcSurfaces :: TVar (M.Map (Ptr C'WlrSurface) GodotWlrootsSurfaceSprite)
  , _gwcOutput :: TVar (Ptr C'WlrOutput) -- possibly needs to be (TVar [Ptr C'WlrOutput])
-- , _gwcNormalLayer :: TVar (Ptr C'WlrLayer) -- so far unused
  }
-}

instance GodotClass GodotWlrootsCompositor where
  godotClassName = "WlrootsCompositor"

instance ClassExport GodotWlrootsCompositor where
  classInit obj  = GodotWlrootsCompositor obj
    <$> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar mempty)
    <*> atomically (newTVar undefined)

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" ready
    , GodotMethod NoRPC "_input" input
    ]

instance HasBaseClass GodotWlrootsCompositor where
  type BaseClass GodotWlrootsCompositor = GodotSpatial
  super (GodotWlrootsCompositor obj _ _ _ _ _ _) = GodotSpatial obj

ready :: GFunc GodotWlrootsCompositor
ready compositor _ = do
  startBaseCompositor compositor
  toLowLevel VariantNil

startBaseCompositor :: GodotWlrootsCompositor -> IO ()
startBaseCompositor compositor = do
  startBaseThread compositor
  startTelemetry (_gwcSurfaces compositor)

startBaseThread :: GodotWlrootsCompositor -> IO ()
startBaseThread compositor = Control.Monad.void $ forkOS $ do
  putStrLn "startBaseThread not implemented yet."
  -- ptrWlDisplay <- [C.block| wl_display* {
  --                     wl_display * display;
  --                     display = wl_display_create();
  --                     assert(display);
  --                     return display;} |]
  -- ptrWlEventLoop <- [C.block| wl_event_loop* {
  --                       wl_event_loop * loop;
  --                       loop = wl_display_get_event_loop($(wl_display *ptrWlDisplay));
  --                       assert(loop);
  --                       return wl_event_loop;} |]
  -- ptrWlrBackend <- [C.block| wl_event_loop* {
  --                       wlr_backend * backend;
  --                       backend = wlr_headless_backend_create($(wl_display* ptrWlDisplay), NULL);
  --                       assert(backend);
  --                       return backend;} |]
  -- TODO: Connect signals with their wl_notify_func_t
          -- wl_list_init(&server.outputs);
          -- server.new_output.notify = new_output_notify;
          -- wl_signal_add(&server.backend->events.new_output, &server.new_output);

          -- if (!wlr_backend_start(server.backend)) {
          --         fprintf(stderr, "Failed to start backend\n");
          --         wl_display_destroy(server.wl_display);
          --         return 1;
          -- }
  -- [C.exp| void { wl_display_run($(wl_display* ptrWlDisplay)); } |]
  -- [C.exp| void { wl_display_destroy($(wl_display* ptrWlDisplay)); } |]
    where newOutputNotify :: (Ptr C'WlListener -> Ptr ())
          newOutputNotify _ = undefined

          outputFrameNotify :: (Ptr C'WlListener -> Ptr ())
          outputFrameNotify = undefined

          outputDestroyNotify :: (Ptr C'WlListener -> Ptr ())
          outputDestroyNotify = undefined


-- TODO: check the origin plane?
moveToUnoccupied :: GodotWlrootsCompositor -> GodotWlrootsSurfaceSprite -> IO ()
moveToUnoccupied gwc gwss = do
  surfaces <- atomically $ readTVar (_gwcSurfaces gwc)
  let elems = filter (\x -> asObj x /= asObj gwss) $ M.elems surfaces

  extents <- forM elems $ \westonSprite -> do
    sprite <- getSprite westonSprite
    aabb   <- G.get_transformed_aabb sprite
    size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
    pos    <- Api.godot_aabb_get_position aabb >>= fromLowLevel
    return (pos, size + pos)

  let minX = minimum $ 0 : map (view $ _1._x) extents
      maxX = maximum $ 0 :  map (view $ _2._x) extents
  sprite <- getSprite gwss
  aabb   <- G.get_aabb sprite
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
      newPos =
        if abs minX < abs maxX
        then V3 (minX - sizeX/2) 0 0
        else V3 (maxX + sizeX/2) 0 0

  G.translate gwss =<< toLowLevel newPos

getSeat :: GodotWlrootsCompositor -> IO (Ptr C'WlrSeat)
getSeat gwc = undefined
  -- do (seat:_) <- atomically (readTVar (_gwcCompositor gwc)) >>= westonCompositorSeats
  --    return seat


input :: GFunc GodotWlrootsCompositor
input self args = do
  (getArg' 0 args :: IO GodotObject)
    >>= asClass GodotInputEventKey "InputEventKey" >>= \case
      Just evk -> do
        -- dsp <- readTVarIO (_gwcWlDisplay self)
        -- kbd <- getKeyboard self
        -- processKeyEvent dsp kbd evk
        putStrLn "WlrInput handling not yet implemented."
        setInputHandled self

      Nothing  -> return () -- not a key
  toLowLevel VariantNil
  -- where
    -- getKeyboard :: GodotWlrootsCompositor -> IO (Ptr C'WlrKeyboard)
    -- getKeyboard gwc = getSeat gwc >>= weston_seat_get_keyboard
