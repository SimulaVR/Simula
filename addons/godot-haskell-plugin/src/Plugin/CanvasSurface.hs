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
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurfaceParent <- getWlrSurface eitherSurface
  depthFirstBaseSurfaces <- case eitherSurface of
    Left wlrXdgSurface -> do
      getDepthFirstXdgSurfaces wlrXdgSurface :: IO [(GodotWlrSurface, Int, Int)]
    Right wlrXWaylandSurface -> do
      freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
      freeChildren' <- mapM (\wlrXWaylandSurfaceFC -> do x <- G.get_x wlrXWaylandSurfaceFC
                                                         y <- G.get_y wlrXWaylandSurfaceFC
                                                         wlrSurface <- G.get_wlr_surface wlrXWaylandSurfaceFC
                                                         return (wlrSurface, x, y))
                            freeChildren
      depthFirstXWaylandSurfaces <- getDepthFirstXWaylandSurfaces wlrXWaylandSurface :: IO [(GodotWlrSurface, Int, Int)]
      return (depthFirstXWaylandSurfaces ++ freeChildren')
  depthFirstWlrSurfaces <- getDepthFirstWlrSurfaces wlrSurfaceParent
  let depthFirstSurfaces = depthFirstBaseSurfaces ++ depthFirstWlrSurfaces

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
                    -- savePngCS (wlrSurface, cs)
                    G.draw_texture cs surfaceTexture renderPosition modulateColor (coerce nullPtr)
                    G.send_frame_done wlrSurface

    getTransparency :: CanvasSurface -> IO Double
    getTransparency cs = do
      gsvs <- readTVarIO (cs ^. csGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      return (realToFrac gsvsTransparency)

-- TODO: All (Int, Int) should be relative to root surface; right now,
-- subsurface coordinates are possibly relative to their immediate parent.
getDepthFirstXWaylandSurfaces :: GodotWlrXWaylandSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstXWaylandSurfaces wlrXWaylandSurface = do
  xwaylandMappedChildrenAndCoords <- getXWaylandMappedChildren wlrXWaylandSurface :: IO [(GodotWlrXWaylandSurface, Int, Int)]
  wlrSurface <- G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface
  foldM appendXWaylandSurfaceAndChildren [(wlrSurface, 0, 0)] xwaylandMappedChildrenAndCoords
  where
        appendXWaylandSurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrXWaylandSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        appendXWaylandSurfaceAndChildren oldList arg@(wlrXWaylandSurface, x, y) = do
           xwaylandChildSurface <- G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface
           appendSurfaceAndChildren oldList (xwaylandChildSurface, x, y)

        appendSurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        appendSurfaceAndChildren oldList arg@(wlrSurface, x, y) = do
           subsurfacesAndCoords <- getSurfaceChildren wlrSurface :: IO [(GodotWlrSurface, Int, Int)]
           foldM appendSurfaceAndChildren (oldList ++ [(wlrSurface, x, y)]) subsurfacesAndCoords

        getSurfaceChildren :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
        getSurfaceChildren wlrSurface = do
          arrayOfChildren <- G.get_children wlrSurface :: IO GodotArray
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          childrenSubsurfaces <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrSubsurface]
          childrenSSX <- mapM G.get_ssx childrenSubsurfaces
          childrenSSY <- mapM G.get_ssy childrenSubsurfaces
          children <- mapM G.getWlrSurface childrenSubsurfaces
          let childrenWithCoords = zip3 children childrenSSX childrenSSY
          Api.godot_array_destroy arrayOfChildren
          return childrenWithCoords

        getXWaylandMappedChildren :: GodotWlrXWaylandSurface -> IO [(GodotWlrXWaylandSurface, Int, Int)]
        getXWaylandMappedChildren wlrXWaylandSurface = do
          arrayOfChildren <- G.get_children wlrXWaylandSurface :: IO GodotArray -- Doesn't return non-mapped children
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXWaylandSurface]
          childrenX <- mapM G.get_x children
          childrenY <- mapM G.get_y children
          let childrenWithCoords = zip3 children childrenX childrenY
          Api.godot_array_destroy arrayOfChildren
          return childrenWithCoords

getDepthFirstXdgSurfaces :: GodotWlrXdgSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstXdgSurfaces wlrXdgSurface = do
  xdgPopups <- getXdgPopups wlrXdgSurface :: IO [(GodotWlrXdgSurface, Int, Int)]
  depthFirstXdgSurfaces  <- foldM appendXdgSurfaceAndChildren [(wlrXdgSurface, 0, 0)] xdgPopups
  mapM convertToWlrSurfaceDepthFirstSurfaces depthFirstXdgSurfaces
  where convertToWlrSurfaceDepthFirstSurfaces :: (GodotWlrXdgSurface, Int, Int) -> IO (GodotWlrSurface, Int, Int)
        convertToWlrSurfaceDepthFirstSurfaces (wlrXdgSurface, x, y) = do
          wlrSurface <- G.get_wlr_surface wlrXdgSurface
          return (wlrSurface, x, y)

        getXdgPopups :: GodotWlrXdgSurface -> IO [(GodotWlrXdgSurface, Int, Int)]
        getXdgPopups wlrXdgSurface = do
          arrayOfChildren <- G.get_children wlrXdgSurface :: IO GodotArray -- Doesn't return non-mapped children
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXdgSurface]
          childrenAsPopups <- mapM G.get_xdg_popup children
          childrenX <- mapM G.get_x childrenAsPopups
          childrenY <- mapM G.get_y childrenAsPopups
          let childrenWithCoords = zip3 children childrenX childrenY
          Api.godot_array_destroy arrayOfChildren
          return childrenWithCoords

        appendXdgSurfaceAndChildren :: [(GodotWlrXdgSurface, Int, Int)] -> (GodotWlrXdgSurface, Int, Int) -> IO [(GodotWlrXdgSurface, Int, Int)]
        appendXdgSurfaceAndChildren oldList arg@(wlrXdgSurface, x, y) = do
          subsurfacesAndCoords <- getXdgPopups wlrXdgSurface :: IO [(GodotWlrXdgSurface, Int, Int)]
          foldM appendXdgSurfaceAndChildren (oldList ++ [(wlrXdgSurface, x, y)]) subsurfacesAndCoords

getDepthFirstWlrSurfaces :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstWlrSurfaces wlrSurface = do
  surfaceChildrenAndCoords <- getSurfaceChildren wlrSurface
  foldM appendSurfaceAndChildren [] surfaceChildrenAndCoords
  where appendSurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        appendSurfaceAndChildren oldList arg@(wlrSurface, x, y) = do
          surfacesAndCoords <- getSurfaceChildren wlrSurface :: IO [(GodotWlrSurface, Int, Int)]
          foldM appendSurfaceAndChildren (oldList ++ [(wlrSurface, x, y)]) surfacesAndCoords

        getSurfaceChildren :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
        getSurfaceChildren wlrSurface = do
          arrayOfChildren <- G.get_children wlrSurface :: IO GodotArray
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          childrenSubsurfaces <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrSubsurface]
          childrenSSX <- mapM G.get_ssx childrenSubsurfaces
          childrenSSY <- mapM G.get_ssy childrenSubsurfaces
          children <- mapM G.getWlrSurface childrenSubsurfaces
          let childrenWithCoords = zip3 children childrenSSX childrenSSY
          Api.godot_array_destroy arrayOfChildren
          return childrenWithCoords