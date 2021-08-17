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
  classMethods =
    [
      func NoRPC "_process" (catchGodot Plugin.CanvasAR._process)
    , func NoRPC "_draw" (catchGodot Plugin.CanvasAR._draw)
    , func NoRPC "_ready" (catchGodot Plugin.CanvasAR._ready)
    ]

_ready :: CanvasAR -> [GodotVariant] -> IO ()
_ready self _ = do
  -- canvasShaderMaterial <- readTVarIO (self ^. carShader)
  -- G.set_material self (safeCast canvasShaderMaterial)
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

  -- For testing purposes:
  -- let m22Rect = V2 (V2 0 0) (V2  1000 1000)
  -- m22Rect' <- toLowLevel m22Rect
  -- redColor <- (toLowLevel $ (rgb 1.0 0.0 0.0) `withOpacity` 0.5) :: IO GodotColor
  -- G.draw_rect car m22Rect' redColor True 1.0 False
