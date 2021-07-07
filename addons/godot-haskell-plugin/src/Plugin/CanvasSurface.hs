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
      func NoRPC "_process" (catchGodot Plugin.CanvasSurface._process)
    , func NoRPC "_draw" (catchGodot Plugin.CanvasSurface._draw)
    , func NoRPC "_ready" (catchGodot Plugin.CanvasSurface._ready)
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

  isEntirelyDamaged <- readTVarIO (gsvs ^. gsvsIsDamaged)
  case isEntirelyDamaged of
    True -> do
      atomically $ writeTVar (gsvs ^. gsvsIsDamaged) False
      mapM (drawWlrSurface cs) depthFirstSurfaces -- Just draw everything
      return ()
    False -> do
      -- Only draw the damaged regions
      let surfaces = fmap (tuple1) depthFirstSurfaces
      let xs = fmap (tuple2) depthFirstSurfaces
      let ys = fmap (tuple3) depthFirstSurfaces
      surfacesVariants <- mapM (\surf -> toLowLevel $ toVariant $ surf) surfaces
      let xsV = fmap toVariant xs
      let ysV = fmap toVariant ys
      xsVariant <- mapM toLowLevel xsV :: IO [GodotVariant]
      ysVariant <- mapM toLowLevel ysV :: IO [GodotVariant]
      surfacesVariantsArray <- toLowLevel surfacesVariants
      xsVariantArray <- toLowLevel xsVariant
      ysVariantArray <- toLowLevel ysVariant
      simulaView <- readTVarIO (gsvs ^. gsvsView)
      let eitherSurface = (simulaView ^. svWlrEitherSurface)
      wlrSurfaceParent <- getWlrSurface eitherSurface >>= validateSurfaceE
      regionsArray <- G.accumulate_damage_regions wlrSurfaceParent surfacesVariantsArray xsVariantArray ysVariantArray
      regions <- fromLowLevel regionsArray >>= mapM fromGodotVariant
      atomically $ writeTVar (gsvs ^. gsvsDamagedRegions) regions

      -- Draw surfaces
      mapM (drawWlrSurfaceRegions cs regions) depthFirstSurfaces
      mapM Api.godot_array_destroy [surfacesVariantsArray, xsVariantArray, ysVariantArray, regionsArray]
      return ()
  where
    tuple1 (a1, b2, c3) = a1
    tuple2 (a1, b2, c3) = b2
    tuple3 (a1, b2, c3) = c3

    savePngCS :: (GodotWlrSurface, CanvasSurface) -> IO ()
    savePngCS arg@((wlrSurface, cs)) = do
      validateSurfaceE wlrSurface
      viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
      viewportSurfaceTexture <- (G.get_texture (viewportSurface :: GodotViewport)) :: IO GodotViewportTexture
      savePng cs viewportSurfaceTexture wlrSurface
      return ()

    drawWlrSurface :: CanvasSurface -> (GodotWlrSurface, Int, Int) -> IO ()
    drawWlrSurface cs (wlrSurface, x, y) = do
      validateSurfaceE wlrSurface
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      G.reference wlrSurface
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

    drawWlrSurfaceRegions :: CanvasSurface -> [GodotRect2] -> (GodotWlrSurface, Int, Int) -> IO ()
    drawWlrSurfaceRegions cs regions (wlrSurface, x, y) = do
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      validateSurfaceE wlrSurface
      do G.reference wlrSurface
         surfaceTexture <- G.get_texture wlrSurface :: IO GodotTexture
         case (validateObject surfaceTexture) of
           Nothing -> return ()
           Just surfaceTexture -> do
               gsvsTransparency <- getTransparency cs
               modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` gsvsTransparency) :: IO GodotColor
               forM_ regions $ \gsvsRegion -> do
                 maybeRegionSurface <- getSurfaceRegion gsvs gsvsRegion (wlrSurface, x, y)
                 maybeGsvsRegionIntersected <- getIntersectedGSVSRegion gsvsRegion (wlrSurface, x, y)
                 case (maybeRegionSurface, maybeGsvsRegionIntersected) of
                   (Just regionSurface, Just gsvsRegionIntersected) -> do
                     bufferDims <- getBufferDimensions wlrSurface
                     gsvsRegion' <- fromLowLevel gsvsRegion
                     regionSurface' <- fromLowLevel regionSurface
                     G.draw_texture_rect_region cs surfaceTexture gsvsRegionIntersected regionSurface modulateColor False (coerce nullPtr) True
                   _ -> return ()
               G.send_frame_done wlrSurface

    getSurfaceRegion :: GodotSimulaViewSprite -> GodotRect2 -> (GodotWlrSurface, Int, Int) -> IO (Maybe GodotRect2)
    getSurfaceRegion gsvs regionGSVS (wlrSurface, x, y) = do
      V2 (V2 rx ry) (V2 rWidth rHeight) <- fromLowLevel regionGSVS
      regionSurfaceRect2 <- toLowLevel $ V2 (V2 (rx - (fromIntegral x)) (ry - (fromIntegral y))) (V2 rWidth rHeight)
      (wlrSurfaceWidth, wlrSurfaceHeight) <- getBufferDimensions wlrSurface
      wlrSurfaceRect2 <- toLowLevel $ V2 (V2 0 0) (V2 (fromIntegral wlrSurfaceWidth) (fromIntegral wlrSurfaceHeight))
      regionSurfaceIntersected <- Api.godot_rect2_clip wlrSurfaceRect2 regionSurfaceRect2
      hasNoArea <- (Api.godot_rect2_has_no_area regionSurfaceIntersected)

      regionSurfaceRect2' <- fromLowLevel regionSurfaceRect2
      regionSurfaceIntersected' <- fromLowLevel regionSurfaceIntersected
      wlrSurfaceRect2' <- fromLowLevel wlrSurfaceRect2
      case hasNoArea of
        0 -> return (Just regionSurfaceIntersected)
        1 -> return Nothing

    -- | Returns intersection of wlrSurface with damage region in gsvs local coordinates
    getIntersectedGSVSRegion :: GodotRect2 -> (GodotWlrSurface, Int, Int) -> IO (Maybe GodotRect2)
    getIntersectedGSVSRegion regionGSVS (wlrSurface, x, y) = do
      (wlrSurfaceWidth, wlrSurfaceHeight) <- getBufferDimensions wlrSurface
      wlrSurfaceRect2 <- toLowLevel $ V2 (V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral wlrSurfaceWidth) (fromIntegral wlrSurfaceHeight))
      regionSurfaceIntersected <- Api.godot_rect2_clip wlrSurfaceRect2 regionGSVS
      hasNoArea <- (Api.godot_rect2_has_no_area regionSurfaceIntersected)
      case hasNoArea of
        0 -> return (Just regionSurfaceIntersected)
        1 -> return Nothing