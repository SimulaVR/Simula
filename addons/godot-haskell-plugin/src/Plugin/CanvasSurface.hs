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

import Plugin.Debug.DamagedRegions
import Plugin.Debug.DamagedRegionTypes
import Plugin.Debug.ProfileHudTypes
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
  passthroughshm <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/CanvasPassthroughShader.tres"
  case (clearshm, passthroughshm) of
    (Just clearshm, Just passthroughshm) -> do
      atomically $ writeTVar (self ^. csClearShader) clearshm
      atomically $ writeTVar (self ^. csPassthroughShader) passthroughshm
      initializeCanvasItemChildrenToBeClearAndPassthroughShaders self clearshm passthroughshm
    _ -> error "Failed to load canvas shaders"

  G.set_process self True
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

initializeCanvasItemChildrenToBeClearAndPassthroughShaders :: CanvasSurface -> GodotShaderMaterial -> GodotShaderMaterial -> IO ()
initializeCanvasItemChildrenToBeClearAndPassthroughShaders cs clearShader passthroughShader = do
  debugPutStrLn "Plugin.CanvasSurface.initializeCanvasItemChildrenToBeClearAndPassthroughShaders"
  visualServer <- getVisualServer cs
  parentCanvasItem <- G.get_canvas_item cs -- returns RID of the underlying cs
  clearCanvasItem <- G.canvas_item_create visualServer
  passthroughCanvasItem <- G.canvas_item_create visualServer
  clearShaderRid <- G.get_rid clearShader
  passthroughShaderRid <- G.get_rid passthroughShader

  -- This doesn't alter scene graph state but instead are like imperative commands
  -- to just tell the CanvasItem how to render
  G.canvas_item_set_parent visualServer clearCanvasItem parentCanvasItem
  G.canvas_item_set_parent visualServer passthroughCanvasItem parentCanvasItem

  -- The clear shader sets (0,0,0,0) for every pixel at the base level
  -- We do this to avoid weird texture effects when wayland clients send us
  -- pixels with alpha values < 1
  G.canvas_item_set_draw_index visualServer clearCanvasItem 0
  G.canvas_item_set_material visualServer clearCanvasItem clearShaderRid

  -- We then use a "texture passthrough" shader for all of the actual surfaces
  -- "Passthrough" (as opposed to a premul shader) since wayland clients already
  -- send us premultiplied pixel data as per wayland documentation
  G.canvas_item_set_draw_index visualServer passthroughCanvasItem 1
  G.canvas_item_set_material visualServer passthroughCanvasItem passthroughShaderRid

  atomically $ writeTVar (cs ^. csClearCanvasItem) clearCanvasItem
  atomically $ writeTVar (cs ^. csPassthroughCanvasItem) passthroughCanvasItem

getVisualServer :: CanvasSurface -> IO GodotVisualServer
getVisualServer cs = do
  debugPutStrLn "Plugin.CanvasSurface.getVisualServer"
  gsvs <- readTVarIO (cs ^. csGSVS)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  readTVarIO (gss ^. gssVisualServer)

_process :: CanvasSurface -> [GodotVariant] -> IO ()
_process self gvArgs =
  profileScope "Plugin.CanvasSurface._process" $ processCanvasSurface self gvArgs

processCanvasSurface :: CanvasSurface -> [GodotVariant] -> IO ()
processCanvasSurface self gvArgs = do
  debugPutStrLn "Plugin.CanvasSurface._process"
  gsvs <- readTVarIO (self ^. csGSVS)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  mapped <- readTVarIO (simulaView ^. svMapped)
  when mapped $ do
    clearViewportIfFullyRedrawing self gsvs
    G.update self
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

clearViewportIfFullyRedrawing :: CanvasSurface -> GodotSimulaViewSprite -> IO ()
clearViewportIfFullyRedrawing cs gsvs = do
  fullRedrawFramesRemaining <- readTVarIO (gsvs ^. gsvsFullRedrawFramesRemaining)
  isEntirelyDamaged <- readTVarIO (gsvs ^. gsvsIsDamaged)
  fullRedrawMillisecondsRemaining <- getGSVSFullRedrawMillisecondsRemaining gsvs
  when (fullRedrawFramesRemaining > 0 || fullRedrawMillisecondsRemaining > 0) $
    atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  when (isEntirelyDamaged || fullRedrawFramesRemaining > 0 || fullRedrawMillisecondsRemaining > 0) $ do
   viewport <- readTVarIO (cs ^. csViewport)
   G.set_clear_mode viewport G.CLEAR_MODE_ONLY_NEXT_FRAME

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
_draw cs gvArgs =
  profileScope "Plugin.CanvasSurface._draw" $ drawCanvasSurfaceFrame cs gvArgs

drawCanvasSurfaceFrame :: CanvasSurface -> [GodotVariant] -> IO ()
drawCanvasSurfaceFrame cs gvArgs = do
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
        fullRedrawMillisecondsRemaining <- getGSVSFullRedrawMillisecondsRemaining gsvs
        let shouldFullRedraw = debugDepthFirstThumbnailsEnabled || isEntirelyDamaged || fullRedrawFramesRemaining > 0 || fullRedrawMillisecondsRemaining > 0
        debugDamageRegions <- if debugDamagedRegionsEnabled
          then do
            regions <- getAccumulatedDamageRegions gsvs depthFirstSurfaces
            atomically $ writeTVar (gsvs ^. gsvsDamagedRegions) regions
            return (Just regions)
          else return Nothing
        visualServer <- getVisualServer cs
        clearCanvasItem <- readTVarIO (cs ^. csClearCanvasItem)
        passthroughCanvasItem <- readTVarIO (cs ^. csPassthroughCanvasItem)

        -- Clears queued draw commands on these canvas item (NOT: clearing pixels)
        -- If we don't do this, then damaged regions from prior frames will
        -- get redrawn again
        G.canvas_item_clear visualServer clearCanvasItem
        G.canvas_item_clear visualServer passthroughCanvasItem
        case shouldFullRedraw of
          True -> do
            atomically $ do
              writeTVar (gsvs ^. gsvsIsDamaged) False
              when (fullRedrawFramesRemaining > 0) $
                writeTVar (gsvs ^. gsvsFullRedrawFramesRemaining) (fullRedrawFramesRemaining - 1)
            drawResults <- mapM (drawWlrSurface cs visualServer passthroughCanvasItem) depthFirstSurfaces -- Just draw everything
            when (not (and drawResults)) $ -- when at least one surface didn't have a valid texture this frame
              markGSVSForFullRedrawFrames gsvs fullRedrawFramesRemaining -- bump to the next frame while retaining our fullRedrawFramesRemaining amount
            rememberDebugDamageRegionsFromCanvas cs gsvs debugDamageRegions
            return ()
          False -> do
            -- Only draw the damaged regions
            regions <- case debugDamageRegions of
              Just regions -> return regions
              Nothing -> do
                regions <- getAccumulatedDamageRegions gsvs depthFirstSurfaces
                atomically $ writeTVar (gsvs ^. gsvsDamagedRegions) regions
                return regions

            -- Draw surfaces
            clearCanvasSurfaceRegions cs visualServer clearCanvasItem regions
            mapM (drawWlrSurfaceRegions cs visualServer passthroughCanvasItem regions) depthFirstSurfaces
            rememberDebugDamageRegionsFromCanvas cs gsvs (Just regions)
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

    drawWlrSurface :: CanvasSurface -> GodotVisualServer -> GodotRid -> (GodotWlrSurface, Int, Int) -> IO Bool
    drawWlrSurface cs visualServer canvasItem (wlrSurface, x, y) = do
      debugPutStrLn "Plugin.CanvasSurface.drawWlrSurface"
      validateSurfaceE wlrSurface
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- getTransparency cs
      modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` gsvsTransparency) :: IO GodotColor
      drewTexture <- withGodotRef (G.get_texture wlrSurface :: IO GodotTexture) $ \surfaceTexture ->
        -- Don't draw when surfaceTexture is NULL to defend against potential flickering
        case validateObject surfaceTexture of
          Nothing -> do
            markGSVSForFullRedrawsByDefaultFrameAmount gsvs
            return False
          Just validSurfaceTexture -> do
            (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
            destinationRect <- toLowLevel $
              V2
                (V2 (fromIntegral x) (fromIntegral y))
                (V2 (fromIntegral bufferWidth) (fromIntegral bufferHeight))
            textureRid <- G.get_rid validSurfaceTexture
            let emptyRid = globalEmptyRid
            -- We build up this command, which allows us to send a draw command directly to our wlroots CanvasItem
            -- If instead we used G.draw_* commands, they would get sent to the parent CanvasSurface
            G.canvas_item_add_texture_rect visualServer canvasItem destinationRect textureRid False modulateColor False emptyRid
            return True
      G.send_frame_done wlrSurface
      return drewTexture

    getTransparency :: CanvasSurface -> IO Double
    getTransparency cs = do
      debugPutStrLn "Plugin.CanvasSurface.getTransparency"
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      return (realToFrac gsvsTransparency)

    clearCanvasSurfaceRegions :: CanvasSurface -> GodotVisualServer -> GodotRid -> [GodotRect2] -> IO ()
    clearCanvasSurfaceRegions _ visualServer canvasItem regions = do
      debugPutStrLn "Plugin.CanvasSurface.clearCanvasSurfaceRegions"
      -- Since we're using the clear shader, all of the pixels drawn are forced to (0,0,0,0) anyway, so
      -- coverColor is really just ceremonial. Note we need the clear shader here since it disables blending
      -- which ensures (0,0,0,0) hard overwrite the damaged region (instead of getting blended, which causes)
      -- weird behavior like we saw with e.g. the synapse launcher.
      coverColor <- (toLowLevel $ (rgb 0.0 0.0 0.0) `withOpacity` 0.0) :: IO GodotColor
      forM_ regions $ \gsvsRegion ->
        G.canvas_item_add_rect visualServer canvasItem gsvsRegion coverColor

    drawWlrSurfaceRegions :: CanvasSurface -> GodotVisualServer -> GodotRid -> [GodotRect2] -> (GodotWlrSurface, Int, Int) -> IO ()
    drawWlrSurfaceRegions cs visualServer canvasItem regions (wlrSurface, x, y) = do
      debugPutStrLn "Plugin.CanvasSurface.drawWlrSurfaceRegions"
      gsvs <- readTVarIO (cs ^. csGSVS)
      validateSurfaceE wlrSurface
      do withGodotRef (G.get_texture wlrSurface :: IO GodotTexture) $ \surfaceTexture ->
           case (validateObject surfaceTexture) of
             Nothing -> markGSVSForFullRedrawsByDefaultFrameAmount gsvs
             Just surfaceTexture -> do
                 gsvsTransparency <- getTransparency cs
                 modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` gsvsTransparency) :: IO GodotColor
                 textureRid <- G.get_rid surfaceTexture
                 let emptyRid = globalEmptyRid
                 forM_ regions $ \gsvsRegion -> do
                   maybeRegionSurface <- getSurfaceRegion gsvs gsvsRegion (wlrSurface, x, y)
                   maybeGsvsRegionIntersected <- getIntersectedGSVSRegion gsvsRegion (wlrSurface, x, y)
                   case (maybeRegionSurface, maybeGsvsRegionIntersected) of
                     (Just regionSurface, Just gsvsRegionIntersected) -> do
                       bufferDims <- getBufferDimensions wlrSurface
                       gsvsRegion' <- fromLowLevel gsvsRegion
                       regionSurface' <- fromLowLevel regionSurface
                       G.canvas_item_add_texture_rect_region visualServer canvasItem gsvsRegionIntersected textureRid regionSurface modulateColor False emptyRid True
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
