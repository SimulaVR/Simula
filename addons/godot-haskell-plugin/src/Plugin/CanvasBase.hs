{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.CanvasBase where

import Control.Exception

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import           Godot.Nativescript
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import Godot.Core.GodotViewport as G

import Godot.Core.GodotVisualServer as G

import Plugin.Types
import Data.Maybe
import Data.Either

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Control.Lens                hiding (Context)

import Data.Typeable

import qualified Data.Map.Strict as M
import qualified Data.List

import Data.Map.Ordered as MO

instance Eq GodotWlrSurface where
  wlrSurface1 == wlrSurface2 = ((coerce wlrSurface1) :: Ptr ()) == ((coerce wlrSurface2) :: Ptr ())

instance Ord GodotWlrSurface where
  wlrSurface1 `compare` wlrSurface2 = ((coerce wlrSurface1) :: Ptr ()) `compare` ((coerce wlrSurface2) :: Ptr ())

instance Eq CanvasBase where
  (==) = (==) `on` _cbObject

instance NativeScript CanvasBase where
  className = "CanvasBase"
  classInit obj = do
    CanvasBase (safeCast obj)
                  <$> atomically (newTVar (error "Failed to initialize CanvasBase."))
                  <*> atomically (newTVar (error "Failed to initialize CanvasBase."))
  classMethods =
    [
      func NoRPC "_process" Plugin.CanvasBase._process
    , func NoRPC "_draw" Plugin.CanvasBase._draw
    , func NoRPC "_ready" Plugin.CanvasBase._ready
    ]

newCanvasBase :: GodotSimulaViewSprite -> IO (CanvasBase)
newCanvasBase gsvs = do
  cb <- "res://addons/godot-haskell-plugin/CanvasBase.gdns"
    & newNS' []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr :: IO CanvasBase

  viewport <- initializeRenderTarget gsvs ViewportBase

  atomically $ writeTVar (_cbGSVS cb) gsvs
  atomically $ writeTVar (_cbViewport cb) viewport

  return cb

_ready :: CanvasBase -> [GodotVariant] -> IO ()
_ready cb _ = do
  G.set_process cb True

_process :: CanvasBase -> [GodotVariant] -> IO ()
_process self args = do
  G.update self
  return ()

_draw :: CanvasBase -> [GodotVariant] -> IO ()
_draw cb _ = do
  gsvs <- readTVarIO (cb ^. cbGSVS)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  -- Draw surfaces from CanvasSurface
  drawCanvasSurface cb gsvs

  -- Draw cursor
  drawCursor cb gsvs

  -- Increment global framecount
  frameCount <- readTVarIO (gsvs ^. gsvsFrameCount)
  atomically $ writeTVar (gsvs ^. gsvsFrameCount) (frameCount + 1)

  where
    getTransparency :: CanvasBase -> IO Double
    getTransparency cb = do
      gsvs <- readTVarIO (cb ^. cbGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      return (realToFrac gsvsTransparency)
    savePngCS :: (GodotWlrSurface, CanvasSurface) -> IO ()
    savePngCS arg@((wlrSurface, cs)) = do
      viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
      viewportSurfaceTexture <- (G.get_texture (viewportSurface :: GodotViewport)) :: IO GodotViewportTexture
      savePng cs viewportSurfaceTexture wlrSurface
      return ()

    drawCanvasSurface :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawCanvasSurface cb gsvs = do
      gss <- readTVarIO (gsvs ^. gsvsServer)
      cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
      viewportSurface <- readTVarIO (cs ^. csViewport)
      viewportSurfaceTexture <- G.get_texture viewportSurface
      renderPosition <- toLowLevel (V2 0 0) :: IO GodotVector2
      gsvsTransparency <- getTransparency cb
      modulateColor <- (toLowLevel $ (rgb 1.0 1.0 (1.0 :: Double)) `withOpacity` gsvsTransparency) :: IO GodotColor

      G.draw_texture cb ((safeCast viewportSurfaceTexture) :: GodotTexture)  renderPosition modulateColor (coerce nullPtr)

    drawCursor :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawCursor cb gsvs = do
      activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
      gss <- readTVarIO (gsvs ^. gsvsServer)
      (maybeWlrSurfaceCursor, maybeCursorTexture) <- readTVarIO (gsvs ^. gsvsCursor)
      maybeScreenshotCursorTexture <- readTVarIO (gss ^. gssScreenshotCursorTexture)
      screenshotModeEnabled <- readTVarIO (gsvs ^. gsvsScreenshotMode)

      -- Fork behavior depending upon whether screenshot mode is enabled
      case (screenshotModeEnabled, maybeWlrSurfaceCursor, maybeScreenshotCursorTexture, maybeCursorTexture)  of
        (False, Just wlrSurfaceCursor, _, _) -> do
           -- Draw client provided cursor
           cursorTexture <- G.get_texture wlrSurfaceCursor
           cursorRenderPosition <- toLowLevel (V2 sx sy) :: IO GodotVector2
           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
           G.draw_texture cb cursorTexture cursorRenderPosition godotColor (coerce nullPtr)
           G.send_frame_done wlrSurfaceCursor
        (False, Nothing, _, Just cursorTexture) -> do
           -- Draw default cursor
           cursorRenderPosition <- toLowLevel (V2 sx sy) :: IO GodotVector2
           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
           G.draw_texture cb cursorTexture cursorRenderPosition godotColor (coerce nullPtr)
        (True, _, Just screenshotCursorTexture, _) -> do
           -- Draw screenshot cursor
           cursorRenderPosition <- toLowLevel (V2 (sx - 16) (sy - 16)) :: IO GodotVector2
           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
           G.draw_texture cb screenshotCursorTexture cursorRenderPosition godotColor (coerce nullPtr)

           screenshotCoords@(origin, end) <- readTVarIO (gsvs ^. gsvsScreenshotCoords)
           case (origin, end) of
             (Just (SurfaceLocalCoordinates (ox, oy)), Nothing) -> do
               -- Allow user to see screenshot region
               activeGSVSCursorPos@(SurfaceLocalCoordinates (cx, cy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
               let sizeX = cx - ox
               let sizeY = cy - oy
               let m22Rect = V2 (V2 ox oy) (V2  sizeX sizeY)
               m22Rect' <- toLowLevel m22Rect
               grayColor <- (toLowLevel $ (rgb 0.0 0.0 0.0) `withOpacity` 0.5) :: IO GodotColor
               G.draw_rect cb m22Rect' grayColor False 2.0 False
             (Just (SurfaceLocalCoordinates (ox, oy)), Just (SurfaceLocalCoordinates (ex, ey))) -> do
               putStrLn $ "Screenshot mode: taking screenshot from (" ++ (show ox) ++ ", " ++ (show oy) ++ ") to (" ++ (show ex) ++ ", " ++ (show ey) ++ ")"

               -- Take screenshot & save to X clipboard
               let m22Rect = V2 (V2 ox oy) (V2 (ex - ox) (ey - oy))
               viewportSurface <- readTVarIO (cb ^. cbViewport) :: IO GodotViewport
               viewportSurfaceTexture <- (G.get_texture (viewportSurface :: GodotViewport)) :: IO GodotViewportTexture
               saveViewportAsPngAndLaunch gsvs viewportSurfaceTexture m22Rect

               -- Disable screenshot
               atomically $ writeTVar (gsvs ^. gsvsScreenshotMode) False
               atomically $ writeTVar (gsvs ^. gsvsScreenshotCoords) (Nothing, Nothing)
             _ -> return ()
        _ -> return ()
