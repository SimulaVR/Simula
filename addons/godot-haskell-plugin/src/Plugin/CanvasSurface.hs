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
_ready self gvArgs = do
  debugPutStrLn "Plugin.CanvasSurface._ready"
  clearshm <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/CanvasClearShader.tres"
  premulshm <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/CanvasPremulShader.tres"
  case (clearshm, premulshm) of
    (Just clearshm, Just premulshm) -> do
      atomically $ writeTVar (self ^. csClearShader) clearshm
      atomically $ writeTVar (self ^. csPremulShader) premulshm
      G.set_material self (safeCast premulshm)
    _ -> error "Failed to load canvas shaders"

  G.set_process self True
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

_process :: CanvasSurface -> [GodotVariant] -> IO ()
_process self gvArgs = do
  debugPutStrLn "Plugin.CanvasSurface._process"
  gsvs <- readTVarIO (self ^. csGSVS)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  mapped <- readTVarIO (simulaView ^. svMapped)
  when mapped $ do
    prepareViewportForDraw self gsvs
    G.update self
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

prepareViewportForDraw :: CanvasSurface -> GodotSimulaViewSprite -> IO ()
prepareViewportForDraw cs gsvs = do
  fullRedrawFramesRemaining <- readTVarIO (gsvs ^. gsvsFullRedrawFramesRemaining)
  when (fullRedrawFramesRemaining > 0) $
    whenM (surfaceHasInsetGeometry gsvs) $ do
      viewport <- readTVarIO (cs ^. csViewport)
      G.set_clear_mode viewport G.CLEAR_MODE_ONLY_NEXT_FRAME
      return ()

getAccumulatedDamageRegions :: GodotSimulaViewSprite -> [(GodotWlrSurface, Int, Int)] -> IO [GodotRect2]
getAccumulatedDamageRegions gsvs depthFirstSurfaces = do
  let surfaces = fmap (\(wlrSurface, _, _) -> wlrSurface) depthFirstSurfaces
  let xs = fmap (\(_, x, _) -> x) depthFirstSurfaces
  let ys = fmap (\(_, _, y) -> y) depthFirstSurfaces
  surfacesVariants <- mapM (\surf -> toLowLevel $ toVariant surf) surfaces
  let xsV = fmap toVariant xs
  let ysV = fmap toVariant ys
  xsVariant <- mapM toLowLevel xsV :: IO [GodotVariant]
  ysVariant <- mapM toLowLevel ysV :: IO [GodotVariant]
  surfacesVariantsArray <- toLowLevel surfacesVariants
  xsVariantArray <- toLowLevel xsVariant
  ysVariantArray <- toLowLevel ysVariant
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = simulaView ^. svWlrEitherSurface
  regionsArray <- withWlrSurface eitherSurface $ \wlrSurfaceParent ->
    G.accumulate_damage_regions wlrSurfaceParent surfacesVariantsArray xsVariantArray ysVariantArray
  regions <- fromLowLevel regionsArray >>= mapM fromGodotVariant
  mapM Api.godot_array_destroy [surfacesVariantsArray, xsVariantArray, ysVariantArray, regionsArray]
  return regions

_draw :: CanvasSurface -> [GodotVariant] -> IO ()
_draw cs gvArgs = do
  debugPutStrLn "Plugin.CanvasSurface._draw"
  gsvs <- readTVarIO (cs ^. csGSVS)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  mapped <- readTVarIO (simulaView ^. svMapped)
  when mapped $
    bracket
      (getDepthFirstSurfaces gsvs)
      destroyWlrSurfacesWithCoords
      (\depthFirstSurfaces -> do
        isEntirelyDamaged <- readTVarIO (gsvs ^. gsvsIsDamaged)
        fullRedrawFramesRemaining <- readTVarIO (gsvs ^. gsvsFullRedrawFramesRemaining)
        case (isEntirelyDamaged || fullRedrawFramesRemaining > 0) of
          True -> do
            atomically $ do
              writeTVar (gsvs ^. gsvsIsDamaged) False
              when (fullRedrawFramesRemaining > 0) $
                writeTVar (gsvs ^. gsvsFullRedrawFramesRemaining) (fullRedrawFramesRemaining - 1)
            drawResults <- mapM (drawWlrSurface cs) depthFirstSurfaces -- Just draw everything
            when (not (and drawResults)) $ -- when at least one surface didn't have a valid texture this frame
              markGSVSForFullRedrawFrames gsvs fullRedrawFramesRemaining -- bump to the next frame while retaining our fullRedrawFramesRemaining amount
            return ()
          False -> do
            -- Only draw the damaged regions
            regions <- getAccumulatedDamageRegions gsvs depthFirstSurfaces
            atomically $ writeTVar (gsvs ^. gsvsDamagedRegions) regions

            -- Draw surfaces
            mapM (drawWlrSurfaceRegions cs regions) depthFirstSurfaces
            return ())
  mapM_ Api.godot_variant_destroy gvArgs
  where
    savePngCS :: (GodotWlrSurface, CanvasSurface) -> IO ()
    savePngCS arg@((wlrSurface, cs)) = do
      debugPutStrLn "Plugin.CanvasSurface.savePngCS"
      validateSurfaceE wlrSurface
      viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
      withGodotRef (G.get_texture (viewportSurface :: GodotViewport) :: IO GodotViewportTexture) $ \viewportSurfaceTexture ->
        savePng cs viewportSurfaceTexture wlrSurface >> return ()

    drawWlrSurface :: CanvasSurface -> (GodotWlrSurface, Int, Int) -> IO Bool
    drawWlrSurface cs (wlrSurface, x, y) = do
      debugPutStrLn "Plugin.CanvasSurface.drawWlrSurface"
      validateSurfaceE wlrSurface
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      gsvsTransparency <- getTransparency cs
      modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` gsvsTransparency) :: IO GodotColor
      renderPosition <- toLowLevel (V2 (fromIntegral x) (fromIntegral y))
      drewTexture <- withGodotRef (G.get_texture wlrSurface :: IO GodotTexture) $ \surfaceTexture ->
        -- Don't draw when surfaceTexture is NULL to defend against potential flickering
        case validateObject surfaceTexture of
          Nothing -> do
            markGSVSForFullRedraws gsvs
            return False
          Just validSurfaceTexture -> do
            G.draw_texture cs validSurfaceTexture renderPosition modulateColor (coerce nullPtr)
            return True
      G.send_frame_done wlrSurface
      return drewTexture

    getTransparency :: CanvasSurface -> IO Double
    getTransparency cs = do
      debugPutStrLn "Plugin.CanvasSurface.getTransparency"
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      return (realToFrac gsvsTransparency)

    drawWlrSurfaceRegions :: CanvasSurface -> [GodotRect2] -> (GodotWlrSurface, Int, Int) -> IO ()
    drawWlrSurfaceRegions cs regions (wlrSurface, x, y) = do
      debugPutStrLn "Plugin.CanvasSurface.drawWlrSurfaceRegions"
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      validateSurfaceE wlrSurface
      do withGodotRef (G.get_texture wlrSurface :: IO GodotTexture) $ \surfaceTexture ->
           case (validateObject surfaceTexture) of
             Nothing -> markGSVSForFullRedraws gsvs
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
      debugPutStrLn "Plugin.CanvasSurface.getSurfaceRegion"
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
      debugPutStrLn "Plugin.CanvasSurface.getIntersectedGSVSRegion"
      (wlrSurfaceWidth, wlrSurfaceHeight) <- getBufferDimensions wlrSurface
      wlrSurfaceRect2 <- toLowLevel $ V2 (V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral wlrSurfaceWidth) (fromIntegral wlrSurfaceHeight))
      regionSurfaceIntersected <- Api.godot_rect2_clip wlrSurfaceRect2 regionGSVS
      hasNoArea <- (Api.godot_rect2_has_no_area regionSurfaceIntersected)
      case hasNoArea of
        0 -> return (Just regionSurfaceIntersected)
        1 -> return Nothing
