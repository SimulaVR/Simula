{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin.CanvasSurface where

import Control.Monad

import Data.List
import Data.Maybe
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
import Data.Map.Ordered

instance NativeScript CanvasSurface where
  className = "CanvasSurface"
  classInit obj = do
    CanvasSurface (safeCast obj)
                  <$> atomically (newTVar (error "Failed to initialize CanvasSurface"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasSurface"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasSurface"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasSurface"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasSurface"))
  classMethods =
    [
      func NoRPC "_process" Plugin.CanvasSurface._process
    , func NoRPC "_draw" Plugin.CanvasSurface._draw
    , func NoRPC "_ready" Plugin.CanvasSurface._ready
    ]

_ready :: CanvasSurface -> [GodotVariant] -> IO ()
_ready self _ = do
  clearshm <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/CanvasClearShader.tres"
  premulshm <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/CanvasPremulShader.tres"
  case (clearshm, premulshm) of
    (Just clearshm, Just premulshm) -> do
      atomically $ writeTVar (self ^. csClearShader) clearshm
      atomically $ writeTVar (self ^. csPremulShader) premulshm
      G.set_material self (safeCast premulshm)
    _ -> error "Failed to load canvas shaders"

  G.set_process self True
  return ()

_process :: CanvasSurface -> [GodotVariant] -> IO ()
_process self args = do
  G.update self
  return ()

_draw :: CanvasSurface -> [GodotVariant] -> IO ()
_draw cs _ = do
  gsvs <- readTVarIO (cs ^. csGSVS)
  depthFirstSurfaces <- getDepthFirstSurfaces gsvs
  mapM (drawWlrSurface cs) depthFirstSurfaces

  return ()
  where
    savePngCS :: (GodotWlrSurface, CanvasSurface) -> IO ()
    savePngCS arg@((wlrSurface, cs)) = do
      viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
      viewportSurfaceTexture <- (G.get_texture (viewportSurface :: GodotViewport)) :: IO GodotViewportTexture
      savePng cs viewportSurfaceTexture wlrSurface
      return ()

    drawWlrSurface :: CanvasSurface -> (GodotWlrSurface, Int, Int) -> IO ()
    drawWlrSurface cs (wlrSurface, x, y) = do
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)

      let isNull = ((unsafeCoerce wlrSurface) == nullPtr)
      case isNull of
        True -> return ()
        False -> do G.reference wlrSurface
                    gsvsTransparency <- getTransparency cs
                    modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` gsvsTransparency) :: IO GodotColor
                    renderPosition <- toLowLevel (V2 (fromIntegral x) (fromIntegral y))
                    surfaceTexture <- G.get_texture wlrSurface :: IO GodotTexture
                    G.draw_texture cs surfaceTexture renderPosition modulateColor (coerce nullPtr)
                    G.send_frame_done wlrSurface

    getTransparency :: CanvasSurface -> IO Double
    getTransparency cs = do
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      return (realToFrac gsvsTransparency)