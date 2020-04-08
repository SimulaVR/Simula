{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.CanvasSurface where

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

-- instance Eq CanvasSurface
--   (==) = (==) `on` _csObject

instance NativeScript CanvasSurface where
  className = "CanvasSurface"
  classInit obj = do
    CanvasSurface (safeCast obj)
                  <$> atomically (newTVar (error "Failed to initialize CanvasSurface"))
                  <*> atomically (newTVar (error "Failed to initialize CanvasSurface"))
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
    _ -> error "Failed to load canvas shaders"

  -- G.set_material self (safeCast premulshm)

  G.set_process self True
  return ()

_process :: CanvasSurface -> [GodotVariant] -> IO ()
_process self args = do
  G.update self
  return ()

_draw :: CanvasSurface -> [GodotVariant] -> IO ()
_draw cs _ = do
  (wlrSurface, x, y) <- readTVarIO (cs ^. csSurface)
  drawWlrSurface cs wlrSurface x y

  where
    drawWlrSurface :: CanvasSurface -> GodotWlrSurface -> Int -> Int -> IO ()
    drawWlrSurface cs wlrSurface x y = do
      let isNull = ((unsafeCoerce wlrSurface) == nullPtr)
      case isNull of
        True -> putStrLn $ "CanvasSurface is null!"
        False -> do surfaceTexture <- G.get_texture wlrSurface :: IO GodotTexture
                    case ((unsafeCoerce surfaceTexture) == nullPtr) of
                      True -> return ()
                      False -> do modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
                                  -- TODO abstract and refactor
                                  regionsArray <- G.get_damage_regions wlrSurface
                                  regions <- fromLowLevel regionsArray >>= mapM fromGodotVariant
                                  Api.godot_array_destroy regionsArray

                                  let basePosition' = V2 (fromIntegral x) (fromIntegral y)
                                  basePosition <- toLowLevel basePosition'

                                  forM_ regions $ \region -> do
                                    regionPosition <- Api.godot_rect2_get_position region -- Relative to wlr_surface
                                    bufferPosition <- Api.godot_vector2_operator_add basePosition regionPosition -- Relative to gsvs
                                    bufferRect <- Api.godot_rect2_new_with_position_and_size bufferPosition =<< Api.godot_rect2_get_size region

                                    premulShm <- readTVarIO (cs ^. csPremulShader)
                                    G.set_material cs (safeCast premulShm)
                                    G.draw_texture_rect_region cs surfaceTexture bufferRect region modulateColor False (coerce nullPtr) True