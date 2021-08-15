{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin.CanvasAR where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Colour
import Data.Colour.SRGB.Linear
import Data.Coerce
import Data.Either
import Unsafe.Coerce
import Linear
import Data.Typeable

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types

import           Plugin.Imports
import           Plugin.Types
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import           Godot.Nativescript
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api
import           Godot.Core.GodotViewport as G
import           Godot.Core.GodotVisualServer as G
import           Godot.Core.GodotEnvironment           as G
import           Control.Lens                hiding (Context)
import qualified Data.Map.Strict as M
import           Data.Map.Ordered

instance NativeScript CanvasAR where
  className = "CanvasAR"
  classInit obj = do
    CanvasAR      (safeCast obj)
                  <$> atomically (newTVar (error "Failed to initialize CanvasAR"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasAR"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasAR"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasAR"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasAR"))
  classMethods =
    [
      func NoRPC "_process" (catchGodot Plugin.CanvasAR._process)
    , func NoRPC "_draw" (catchGodot Plugin.CanvasAR._draw)
    , func NoRPC "_ready" (catchGodot Plugin.CanvasAR._ready)
    ]

_ready :: CanvasAR -> [GodotVariant] -> IO ()
_ready self _ = do
  canvasLayer <- unsafeInstance GodotCanvasLayer "CanvasLayer"
  G.set_layer canvasLayer (-1)
  atomically $ writeTVar (self ^. carCanvasLayer) canvasLayer

  cameraServer <- getSingleton GodotCameraServer "CameraServer"
  numCameras <- G.get_feed_count cameraServer
  putStrLn $ "CameraServer get_feed_count: " ++ (show numCameras)
  when (numCameras > 0) $ G.get_feed cameraServer 0 >>= \cam -> G.set_active cam True

  cameraTexture <- unsafeInstance GodotCameraTexture "CameraTexture"
  G.set_camera_feed_id cameraTexture 1
  atomically $ writeTVar (self ^. carCameraTexture) cameraTexture

  canvasShaderMaterial <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/CanvasARShader.tres"
  case canvasShaderMaterial of
    Just canvasShaderMaterial -> do
      atomically $ writeTVar (self ^. carShader ) canvasShaderMaterial
      G.set_material self (safeCast canvasShaderMaterial)
    _ -> error "Failed to load CanvasAR shader!"

  G.set_layer canvasLayer (-1)

  gss <- readTVarIO (self ^. carGSS)
  -- carCanvasLayer <- readTVarIO (car ^. carCanvasLayer)
  -- let carCanvasItem = (car ^. carObject)
  addChild gss canvasLayer
  addChild ((safeCast canvasLayer) :: GodotNode) ((safeCast self) :: GodotNode)

  G.set_process self True
  return ()

_process :: CanvasAR -> [GodotVariant] -> IO ()
_process self args = do
  G.update self
  return ()

_draw :: CanvasAR -> [GodotVariant] -> IO ()
_draw car _ = do
  cameraTexture <- readTVarIO (car ^. carCameraTexture)
  modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
  renderPosition <- toLowLevel (V2 0 0)
  G.draw_texture car (safeCast cameraTexture) renderPosition modulateColor (coerce nullPtr)
