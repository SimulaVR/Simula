{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Wlroots (GodotWlrootsCompositor(..)) where

import Plugin.WaylandTypes
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

initializeCppSimulaCtx
C.verbatim "#define WLR_USE_UNSTABLE"
C.include "<wayland-server.h>"
C.include "<wlr/backend.h>"
C.include "<wlr/render/wlr_renderer.h>"
C.include "<wlr/types/wlr_cursor.h>"
C.include "<wlr/types/wlr_compositor.h>"
C.include "<wlr/types/wlr_data_device.h>"
C.include "<wlr/types/wlr_input_device.h>"
C.include "<wlr/types/wlr_keyboard.h>"
C.include "<wlr/types/wlr_matrix.h>"
C.include "<wlr/types/wlr_output.h>"
C.include "<wlr/types/wlr_output_layout.h>"
C.include "<wlr/types/wlr_pointer.h>"
C.include "<wlr/types/wlr_seat.h>"
C.include "<wlr/types/wlr_xcursor_manager.h>"
-- C.include "<wlr/types/wlr_xdg_shell.h>" -- nix presently lacks "xdg-shell-protocol.h"
C.include "<wlr/util/log.h>"
C.include "<xkbcommon/xkbcommon.h>"

C.include "<wlr/backend/headless.h>"

data GodotWlrootsCompositor = GodotWlrootsCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar (Ptr C'WlrCompositor)
  , _gwcWlDisplay :: TVar (Ptr C'WlDisplay)
  , _gwcWlEventLoop :: TVar (Ptr C'WlEventLoop)
  , _gwcBackEnd :: TVar (Ptr C'WlrBackend)
  , _gwcSurfaces :: TVar (M.Map (Ptr C'WlrSurface) GodotWlrootsSurfaceSprite)
  , _gwcOutput :: TVar (Ptr C'WlrOutput) -- possibly needs to be (TVar [Ptr C'WlrOutput])
  , _gwcNormalLayer :: TVar (Ptr C'WlrLayer)
  }

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
    <*> atomically (newTVar undefined)

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" ready
    , GodotMethod NoRPC "_input" input
    ]

instance HasBaseClass GodotWlrootsCompositor where
  type BaseClass GodotWlrootsCompositor = GodotSpatial
  super (GodotWlrootsCompositor obj _ _ _ _ _ _ _) = GodotSpatial obj

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
  ptrWlDisplay <- [C.block| wl_display* {
                      wl_display * display;
                      display = wl_display_create();
                      assert(display);
                      return display;} |]
  ptrWlEventLoop <- [C.block| wl_event_loop* {
                        wl_event_loop * loop;
                        loop = wl_display_get_event_loop($(wl_display *ptrWlDisplay));
                        assert(loop);
                        return wl_event_loop;} |]
  ptrWlrBackend <- [C.block| wl_event_loop* {
                        wlr_backend * backend;
                        backend = wlr_headless_backend_create($(wl_display* ptrWlDisplay), NULL);
                        assert(backend);
                        return backend;} |]
  -- TODO: Connect signals with their wl_notify_func_t
          -- wl_list_init(&server.outputs);
          -- server.new_output.notify = new_output_notify;
          -- wl_signal_add(&server.backend->events.new_output, &server.new_output);

          -- if (!wlr_backend_start(server.backend)) {
          --         fprintf(stderr, "Failed to start backend\n");
          --         wl_display_destroy(server.wl_display);
          --         return 1;
          -- }
  [C.exp| void { wl_display_run($(wl_display* ptrWlDisplay)); } |]
  [C.exp| void { wl_display_destroy($(wl_display* ptrWlDisplay)); } |]
  putStrLn "DEBUG: startBaseThread complete."
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
