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
  gsvs <- readTVarIO (cb ^. cbGSVS)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  case wlrEitherSurface of
     Left xdgSurface -> return ()
     Right wlrXWaylandSurface -> do newSurfacesAndCoords <- getDepthFirstSurfaces wlrXWaylandSurface :: IO [(GodotWlrSurface, Int, Int)]
                                    surfaceMap <- newSurfaceMap gsvs newSurfacesAndCoords
                                    atomically $ writeTVar (_gsvsSurfaceMap gsvs) surfaceMap
  G.set_process cb True

_process :: CanvasBase -> [GodotVariant] -> IO ()
_process self args = do
  -- putStrLn "process in CanvasBase"

  G.update self
  return ()

_draw :: CanvasBase -> [GodotVariant] -> IO ()
_draw cb _ = do
  gsvs <- readTVarIO (cb ^. cbGSVS)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  -- Update surfaces
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  case wlrEitherSurface of
     Left xdgSurface -> return ()
     Right wlrXWaylandSurface -> do newSurfacesAndCoords <- getDepthFirstSurfaces wlrXWaylandSurface :: IO [(GodotWlrSurface, Int, Int)]
                                    -- oldSurfaceMap <- newSurfaceMap gsvs newSurfacesAndCoords
                                    oldSurfaceMap <- readTVarIO (gsvs ^. gsvsSurfaceMap)
                                    newSurfaceMap <- mergeSurfaceMap gsvs oldSurfaceMap newSurfacesAndCoords
                                    atomically $ writeTVar (_gsvsSurfaceMap gsvs) newSurfaceMap

  -- Draw surfaces from CanvasSurface
  drawSurfaces cb gsvs

  -- Draw free children
    -- updateFreeChildrenCoordinates -- possibly needed down the road, in case free child coordinates change
  drawFreeChildren cb gsvs

  -- Draw cursor
  drawCursor cb gsvs
  where
    drawFreeChildren :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawFreeChildren cb gsvs = do
      gss <- readTVarIO (gsvs ^. gsvsServer)
      freeChildren <- readTVarIO (gsvs  ^. gsvsFreeChildren)

      mapM (drawFreeChild cb) freeChildren
      return ()
    drawFreeChild :: CanvasBase -> CanvasSurface -> IO ()
    drawFreeChild cb cs = do
      (wlrSurface, _, _) <- readTVarIO (cs ^. csSurface)
      drawSurface cb (wlrSurface, cs)
    savePngCS :: (GodotWlrSurface, CanvasSurface) -> IO ()
    savePngCS arg@((wlrSurface, cs)) = do
      viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
      viewportSurfaceTexture <- (G.get_texture (viewportSurface :: GodotViewport)) :: IO GodotViewportTexture
      savePng cs viewportSurfaceTexture wlrSurface
      return ()

    drawSurfaces :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawSurfaces cb gsvs = do
      gss <- readTVarIO (gsvs ^. gsvsServer)
      surfaceMap <- readTVarIO (gsvs  ^. gsvsSurfaceMap)
      let surfaces = MO.assocs surfaceMap

      mapM (drawSurface cb) surfaces
      return ()

    drawSurface :: CanvasBase -> (GodotWlrSurface, CanvasSurface) -> IO ()
    drawSurface cb arg@(wlrSurface, cs) = do
      viewportSurface <- readTVarIO (cs ^. csViewport)
      viewportSurfaceTexture <- G.get_texture viewportSurface

      renderPosition <- toLowLevel (V2 0 0) :: IO GodotVector2
      modulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
      G.draw_texture cb ((safeCast viewportSurfaceTexture) :: GodotTexture)  renderPosition modulateColor (coerce nullPtr)

    drawCursor :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawCursor cb gsvs = do
      activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
      gss <- readTVarIO (gsvs ^. gsvsServer)
      maybeCursorTexture <- readTVarIO (gss ^. gssCursorTexture)
      case maybeCursorTexture of
        Nothing -> putStrLn "No cursor texture!"
        Just cursorTexture -> do cursorRenderPosition <- toLowLevel (V2 sx sy) :: IO GodotVector2
                                 godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
                                 G.draw_texture cb cursorTexture cursorRenderPosition godotColor (coerce nullPtr)

-- TODO: All (Int, Int) should be relative to root surface; right now, subsurface coordinates are possibly relative to their immediate parent.
getDepthFirstSurfaces :: GodotWlrXWaylandSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstSurfaces wlrXWaylandSurface = do
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

        -- appendSubsurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        -- appendSubsurfaceAndChildren oldList arg@(wlrSubsurface, ssx, ssy)  = do
        --    subsurfacesAndCoords <- getSubsurfaceChildren wlrSubsurface
        --    wlrSurface <- G.getWlrSurface (wlrSubsurface :: GodotWlrSubsurface)
        --    foldM appendSubsurfaceAndChildren (oldList ++ [(wlrSurface, ssx, ssy)]) subsurfacesAndCoords

        getSurfaceChildren :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
        getSurfaceChildren wlrSurface = do
          arrayOfChildren <- G.get_children wlrSurface :: IO GodotArray
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromLowLevel' arrayOfChildren
          childrenSubsurfaces <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrSubsurface]
          childrenSSX <- mapM G.get_ssx childrenSubsurfaces
          childrenSSY <- mapM G.get_ssy childrenSubsurfaces

          children <- mapM G.getWlrSurface childrenSubsurfaces

          let childrenWithCoords = zip3 children childrenSSX childrenSSY

          Api.godot_array_destroy arrayOfChildren
          return childrenWithCoords

        -- getSubsurfaceChildren :: GodotWlrSubsurface -> IO [(GodotWlrSurface, Int, Int)]
        -- getSubsurfaceChildren wlrSubsurface = do
        --   arrayOfChildren <- G.get_children wlrSubsurface :: IO GodotArray
        --   numChildren <- Api.godot_array_size arrayOfChildren
        --   arrayOfChildrenGV <- fromLowLevel' arrayOfChildren
        --   children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrSubsurface]

        --   childrenSSX <- mapM G.get_ssx children
        --   childrenSSY <- mapM G.get_ssy children
        --   let childrenWithCoords = zip3 children childrenSSX childrenSSY


        --   Api.godot_array_destroy arrayOfChildren
        --   return childrenWithCoords

        getXWaylandMappedChildren :: GodotWlrXWaylandSurface -> IO [(GodotWlrXWaylandSurface, Int, Int)]
        getXWaylandMappedChildren wlrXWaylandSurface = do
          arrayOfChildren <- G.get_children wlrXWaylandSurface :: IO GodotArray -- Doesn't return non-mapped children
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromLowLevel' arrayOfChildren
          children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXWaylandSurface]

          childrenX <- mapM G.get_x children
          childrenY <- mapM G.get_y children
          let childrenWithCoords = zip3 children childrenX childrenY

          Api.godot_array_destroy arrayOfChildren
          return childrenWithCoords

        fromLowLevel' vs = do
          size <- fromIntegral <$> Api.godot_array_size vs
          forM [0..size-1] $ Api.godot_array_get vs

-- To be called the first frame
newSurfaceMap :: GodotSimulaViewSprite -> [(GodotWlrSurface, Int, Int)] -> IO SurfaceMap
newSurfaceMap gsvs lst = do
  let surfaces = fmap fst3 lst
  canvasSurfaces <- mapM (newCanvasSurface gsvs) lst
  let surfaceMapList = zip surfaces canvasSurfaces
  -- Assumes `deriving instance Ord GodotWlrSurface`
  let surfaceMap = MO.fromList surfaceMapList
  return surfaceMap

-- To be called each frame
mergeSurfaceMap :: GodotSimulaViewSprite -> SurfaceMap -> [(GodotWlrSurface, Int, Int)] -> IO SurfaceMap
mergeSurfaceMap gsvs oldSurfaceMap depthFirstSurfaces = do
  let surfaces = fmap fst3 depthFirstSurfaces
  newCanvasSurfaces <- mapM (getOrCreateCanvasSurface gsvs oldSurfaceMap) depthFirstSurfaces
  let newSurfaceMapList = zip surfaces newCanvasSurfaces
  let newSurfaceMap = MO.fromList newSurfaceMapList

  let deleteThisSurfaceMap = (MO.\\) oldSurfaceMap newSurfaceMap
  let deleteTheseCanvasSurfaces = fmap snd (MO.assocs deleteThisSurfaceMap)
  mapM_ destroyCS deleteTheseCanvasSurfaces

  return newSurfaceMap
  where getOrCreateCanvasSurface :: GodotSimulaViewSprite -> SurfaceMap -> (GodotWlrSurface, Int, Int) -> IO CanvasSurface
        getOrCreateCanvasSurface gsvs oldSurfaceMap key@(wlrSurfaceKey, x, y) = do
          let maybeCS = MO.lookup wlrSurfaceKey oldSurfaceMap
          cs <- case maybeCS of
                    Nothing -> newCanvasSurface gsvs key
                    Just cs -> return cs
          return cs

        destroyCS :: CanvasSurface -> IO ()
        destroyCS cs = do
          gsvs <- readTVarIO (cs ^. csGSVS)
          viewport <- readTVarIO (cs ^. csViewport)
          G.set_process cs False
          removeChild viewport cs
          removeChild gsvs viewport
          Api.godot_object_destroy (safeCast cs)