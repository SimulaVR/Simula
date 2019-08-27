
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.SimulaViewSprite where

import Control.Exception

import Debug.C

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Extra.Register
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import Plugin.Types
import Data.Maybe
import Data.Either

-- import           Data.Vector.V2

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Control.Lens                hiding (Context)

import Data.Typeable

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
-- import           Graphics.Wayland.WlRoots.Compositor
import           Graphics.Wayland.WlRoots.Output
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.WlRoots.Backend
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.Render
-- import           Graphics.Wayland.WlRoots.Render.Color
-- import           Graphics.Wayland.WlRoots.OutputLayout
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Seat
-- import           Graphics.Wayland.WlRoots.Cursor
-- import           Graphics.Wayland.WlRoots.XCursorManager
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Input.Keyboard
-- import           Graphics.Wayland.WlRoots.Input.Pointer
-- import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.Input.Buttons
-- import           Graphics.Wayland.WlRoots.Box
import qualified Data.Map.Strict as M

C.initializeSimulaCtxAndIncludes

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance GodotClass GodotSimulaViewSprite where
  godotClassName = "SimulaViewSprite"

instance ClassExport GodotSimulaViewSprite where
  classInit obj =
    GodotSimulaViewSprite obj
                  <$> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar True)
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  -- <*> atomically (newTVar False)
  classExtends = "RigidBody"
  classMethods =
    [ GodotMethod NoRPC "_input_event" inputEvent
    , GodotMethod NoRPC "_ready" ready

    , GodotMethod NoRPC "_handle_destroy" _handle_destroy -- Connected in SimulaServer.hs
    , GodotMethod NoRPC "_handle_map" _handle_map         -- Connected in SimulaServer.hs
    , GodotMethod NoRPC "_handle_unmap" _handle_unmap     -- Connected in SimulaServer.hs
    , GodotMethod NoRPC "_process" Plugin.SimulaViewSprite._process
    ]

  -- Test:
  classSignals = [ signal "map" [("gsvs", GodotVariantTypeObject)]
                 , signal "unmap" [("gsvs", GodotVariantTypeObject)]
                 ]

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ _ _) = GodotRigidBody obj

-- | Updates the GodotSimulaViewSprite state (including updating its texture).
-- | Intended to be called every frame.
updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  -- putStrLn "updateSimulaViewSprite"

  -- Update sprite texture; doesn't yet include popups or other subsurfaces.
  drawParentWlrSurfaceTextureOntoSprite gsvs
    -- useViewportToDrawParentSurface gsvs -- Causes _draw() error
    -- drawSurfacesOnSprite gsvs

  -- Set extents
  setExtents gsvs

  -- Move if needed
  whenM (spriteShouldMove gsvs) $ do
    atomically $ writeTVar (_gsvsShouldMove gsvs) False
    moveToUnoccupied gsvs

  where
        -- As the name makes clear, this function *only* draws the parent WlrSurface
        -- onto a GodotSimulaViewSprite's Sprite3D field. It doesn't include popups or
        -- any other subsurfaces. This is a temporary hack that needs fixed.
        drawParentWlrSurfaceTextureOntoSprite :: GodotSimulaViewSprite -> IO ()
        drawParentWlrSurfaceTextureOntoSprite gsvs = do
          -- Get state
          sprite3D <- readTVarIO (gsvs ^. gsvsSprite)
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
          wlrSurface <- case wlrEitherSurface of
                                Left wlrXdgSurface       -> G.get_wlr_surface wlrXdgSurface
                                Right wlrXWaylandSurface -> do
                                  -- TODO: Complete this experiment
                                  -- children <- G.get_children wlrXWaylandSurface
                                  -- size' <- Api.godot_array_size children
                                  -- putStrLn $ "# of WlrXWayland Surfaces: " ++ (show size')
                                  G.get_wlr_surface wlrXWaylandSurface
          parentWlrTexture <- G.get_texture wlrSurface


          -- saveTextureToDisk wlrXdgSurface parentWlrTexture -- Causes crash
          -- Set Sprite3D texture
         
          -- Attempt to force maximize windows doesn't work:
          -- wlrXdgSurfaceToplevel <- G.get_xdg_toplevel wlrXdgSurface
          -- G.set_maximized wlrXdgSurfaceToplevel True -- Doesn't seem to work
          rid <- G.get_rid parentWlrTexture
          visualServer <- getSingleton GodotVisualServer "VisualServer"
          -- Enable everything but mipmapping (since this causes old textures get to interpolated
          -- with updated textures when far enough from the user).
          G.texture_set_flags visualServer rid 6
          G.set_texture sprite3D parentWlrTexture

          -- Tell client surface it should start rendering the next frame
          G.send_frame_done wlrSurface

        setExtents :: GodotSimulaViewSprite -> IO ()
        setExtents gsvs = do
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = simulaView ^. svWlrEitherSurface
          -- Get state
          sprite <- atomically $ readTVar (_gsvsSprite gsvs)
          aabb <- G.get_aabb sprite
          size <- godot_aabb_get_size aabb
          shape <- atomically $ readTVar (_gsvsShape gsvs)

          -- Compute new extents
          size' <- godot_vector3_operator_divide_scalar size 2
          (V3 x y z)  <- fromLowLevel size'
          -- size2d  <- toLowLevel (V2 x y) :: IO GodotVector2
          -- size2d' <- toLowLevel (V2 500 500) :: IO GodotVector2
          --G.set_size wlrXWaylandSurface size2d'

          -- Set extents
          G.set_extents shape size'

        spriteShouldMove :: GodotSimulaViewSprite -> IO Bool
        spriteShouldMove gsvs = do
          en <- atomically $ readTVar (_gsvsShouldMove gsvs)
          if en then do
            -- putStrLn "spriteShouldMove"
            sprite <- atomically $ readTVar (_gsvsSprite gsvs)
            aabb <- G.get_aabb sprite
            size <- godot_aabb_get_size aabb
            vsize <- fromLowLevel size
            return (vsize > 0)
            else return False

        -- TODO: check the origin plane?
        moveToUnoccupied :: GodotSimulaViewSprite -> IO ()
        moveToUnoccupied gsvs = do
          -- putStrLn "moveToUnoccupied"
          gss <- readTVarIO (gsvs ^. gsvsServer)
          viewMap <- atomically $ readTVar (_gssViews gss)
          let otherGsvs = filter (\x -> asObj x /= asObj gsvs) $ M.elems viewMap

          extents <- forM otherGsvs $ \viewSprite -> do
            sprite <- atomically $ readTVar (gsvs ^. gsvsSprite) -- getSprite viewSprite
            aabb   <- G.get_transformed_aabb sprite
            size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
            pos    <- Api.godot_aabb_get_position aabb >>= fromLowLevel
            return (pos, size + pos)

          let minX = minimum $ 0 : map (view $ _1._x) extents
              maxX = maximum $ 0 :  map (view $ _2._x) extents
          sprite <- atomically $ readTVar (gsvs ^. gsvsSprite)
          aabb   <- G.get_aabb sprite
          size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
          let sizeX  = size ^. _x
              newPos =
                if abs minX < abs maxX
                then V3 (minX - sizeX/2) 0 0
                else V3 (maxX + sizeX/2) 0 0

          G.translate gsvs =<< toLowLevel newPos

ready :: GFunc GodotSimulaViewSprite
ready self _ = do
  -- putStrLn "ready in SimulaViewSprite.hs"
  G.set_mode self RigidBody.MODE_KINEMATIC
  toLowLevel VariantNil

inputEvent :: GFunc GodotSimulaViewSprite
inputEvent self args = do
  -- putStrLn "inputEvent in SimulaViewSprite.hs"
  case toList args of
    [_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] ->  do
      ev <- fromGodotVariant evObj
      clickPos <- fromGodotVariant clickPosObj
      processInputEvent self ev clickPos
      godot_object_destroy ev
    _ -> putStrLn "expected 5 arguments in _input_event"
  toLowLevel VariantNil

-- Needs more descriptive type constructor
data InputEventType
  = Motion
  | Button Bool -- (GodotIsPressed Bool)       ?
           Int  -- (GodotButtonDescriptor Int) ?

-- | Handles mouse (i.e., non-VR controller) events in pancake mode.
processInputEvent :: GodotSimulaViewSprite -> GodotObject -> GodotVector3 -> IO ()
processInputEvent gsvs ev clickPos = do
  -- putStrLn "processInputEvent"
  whenM (ev `isClass` "InputEventMouseMotion") $ processClickEvent gsvs Motion clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processClickEvent gsvs (Button pressed button) clickPos

-- | This function used in `_on_WlrXdgShell_new_surface` (where we have access
-- | to GodotSimulaServer + GodotWlrXdgSurface).
newGodotSimulaViewSprite :: GodotSimulaServer -> SimulaView -> IO (GodotSimulaViewSprite)
newGodotSimulaViewSprite gss simulaView = do
  -- putStrLn "newGodotSimulaViewSprite"
  gsvs <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & newNS' []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr :: IO GodotSimulaViewSprite -- w/_gsvsObj populated + mempty TVars

  godotSprite3D <- unsafeInstance GodotSprite3D "Sprite3D"
  G.set_pixel_size godotSprite3D 0.001
  G.add_child gsvs (safeCast godotSprite3D) True
  G.set_flip_h godotSprite3D True

  -- HACK: Set transparency to False to ensure that textures never disappear
  G.set_draw_flag godotSprite3D 0 False -- https://github.com/godotengine/godot/blob/89bcfa4b364e1edc8e175f766b50d145864eb159/scene/3d/sprite_3d.h#L44:7

  godotBoxShape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gsvs (safeCast gsvs)
  G.shape_owner_add_shape gsvs ownerId (safeCast godotBoxShape)

  -- atomically $ writeTVar (_gsvsObj       gss) gsObj'      -- :: GodotObject (filled in classInit)
  atomically $ writeTVar (_gsvsServer    gsvs) gss           -- :: TVar GodotSimulaServer
  -- atomically $ writeTVar (_gsvsShouldMoe gsvs) gsvsShouldMoe' -- :: TVar Bool   (filled in classInit)
  atomically $ writeTVar (_gsvsSprite    gsvs) godotSprite3D -- :: TVar GodotSprite3D
  atomically $ writeTVar (_gsvsShape     gsvs) godotBoxShape -- :: TVar GodotBoxShape
  atomically $ writeTVar (_gsvsView      gsvs) simulaView    -- :: TVar SimulaView

  -- Initialize and load render target into _gsvsViewport field
    -- let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
    -- renderTarget <- initializeRenderTarget wlrXdgSurface
    -- atomically $ writeTVar (_gsvsViewport gsvs) renderTarget
  G.set_process gsvs False

--  updateSimulaViewSprite gsvs -- Now we update everything

  return gsvs

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView)
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer) -- ^. gssWlrSeat)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  case wlrEitherSurface of
    Left wlrXdgSurface -> do wlrSurface  <- G.get_wlr_surface wlrXdgSurface
                             wlrSurface' <- asGodotVariant wlrSurface
                             toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
                             isGodotTypeNull wlrSurface
                             G.set_activated toplevel True
                             G.keyboard_notify_enter wlrSeat wlrSurface'
    Right wlrXWaylandSurface -> do wlrSurface  <- G.get_wlr_surface wlrXWaylandSurface
                                   wlrSurface' <- asGodotVariant wlrSurface
                                   isGodotTypeNull wlrSurface
                                   G.set_activated wlrXWaylandSurface True
                                   G.keyboard_notify_enter wlrSeat wlrSurface'

-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
-- |
-- | TODO: Change this horifically named function.
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> GodotVector3
                  -> IO ()
processClickEvent gsvs evt clickPos = do
  -- putStrLn "processClickEvent"
  -- Get state
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) <- getSurfaceLocalCoordinates clickPos
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) <-
    case wlrEitherSurface of
      Right godotWlrXWaylandSurface -> do
        getXWaylandSubsurfaceAndCoords godotWlrXWaylandSurface surfaceLocalCoords
      Left godotWlrXdgSurface -> getXdgSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords
  -- -- Send events
  case evt of
    Motion                -> do pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                                pointerNotifyMotion wlrSeat subSurfaceLocalCoords
                                keyboardNotifyEnter wlrSeat godotWlrSurface -- HACK: shouldn't have to call this every frame we're pointing at a surface
    Button pressed button ->    pointerNotifyButton wlrSeat evt

  pointerNotifyFrame wlrSeat

  where
    getSurfaceLocalCoordinates :: GodotVector3 -> IO (SurfaceLocalCoordinates)
    getSurfaceLocalCoordinates clickPos = do
      lpos <- G.to_local gsvs clickPos >>= fromLowLevel
      sprite <- atomically $ readTVar (_gsvsSprite gsvs)
      aabb <- G.get_aabb sprite
      size <- godot_aabb_get_size aabb >>= fromLowLevel
      let topleftPos =
            V2 (size ^. _x / 2 - lpos ^. _x) (size ^. _y / 2 - lpos ^. _y)
      let scaledPos = liftI2 (/) topleftPos (size ^. _xy)
      rect <- G.get_item_rect sprite
      recSize <- godot_rect2_get_size rect >>= fromLowLevel
      let coords = liftI2 (*) recSize scaledPos
      -- coords = surface coordinates in pixel with (0,0) at top left
      let sx = fromIntegral $ truncate (1 * coords ^. _x) -- 256 was old factor
          sy = fromIntegral $ truncate (1 * coords ^. _y) -- 256 was old factor
      clickPos' <- fromLowLevel clickPos
      -- putStrLn $ "getSurfaceLocalCoordinates clickPos: " ++ (show clickPos')
      -- putStrLn $ "getSurfaceLocalCoordinates (sx, sy):" ++ "(" ++ (show sx) ++ ", " ++ (show sy) ++ ")"
      return (SurfaceLocalCoordinates (sx, sy))

    -- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
    -- | TODO: This function just returns parent surface/coords. Fix!
    -- | TODO: Use _xdg_surface_at*
    getXdgSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
    getXdgSubsurfaceAndCoords wlrXdgSurface (SurfaceLocalCoordinates (sx, sy)) = do
      -- BROKEN since G.surface_at is broken (which causes G.get_surface to be broken).
      -- wlrSurfaceAtResult   <- G.surface_at  wlrXdgSurface sx sy -- Not NULL itself, but the wlr_surface within this data structure is NULL due to ~wlr_xdg_surface_surface_at(wlr_xdg_surface, ..)~ being NULL (but not because wlr_xdg_surface is NULL).
      -- wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult
      -- ssx                  <- G.get_sub_x wlrSurfaceAtResult
      -- ssy                  <- G.get_sub_y wlrSurfaceAtResult
      -- let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
      -- return (wlrSurfaceSubSurface, ssCoordinates)
      wlrSurfaceParent <- G.get_wlr_surface wlrXdgSurface            -- hack!
      return (wlrSurfaceParent, SubSurfaceLocalCoordinates (sx, sy)) -- hack!

    -- | TODO: Use wlr_surface_surface_at
    getXWaylandSubsurfaceAndCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
    getXWaylandSubsurfaceAndCoords wlrXWaylandSurface (SurfaceLocalCoordinates (sx, sy)) = do
      wlrSurfaceParent <- G.get_wlr_surface wlrXWaylandSurface
      return (wlrSurfaceParent, SubSurfaceLocalCoordinates (sx, sy)) -- hack!

    -- | Let wlroots know we have entered a new surface. We can safely call this
    -- | over and over (wlroots checks if we've called it already for this surface
    -- | and, if so, returns early.
    pointerNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
    pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (ssx, ssy)) = do
      let maybeWlrSurfaceGV = (fromVariant ((toVariant wlrSurface) :: Variant 'GodotTy) :: Maybe GodotVariant)
      case maybeWlrSurfaceGV of
          Nothing -> putStrLn "Failed to convert GodotWlrSurface to GodotVariant!"
          Just wlrSurfaceGV -> do G.pointer_notify_enter wlrSeat wlrSurfaceGV ssx ssy -- Causing a crash
                                  -- putStrLn $ "G.point_notify_enter: " ++ "(" ++ (show ssx) ++ ", " ++ (show ssy) ++ ")"

    keyboardNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> IO ()
    keyboardNotifyEnter wlrSeat wlrSurface = do
      let maybeWlrSurfaceGV = (fromVariant ((toVariant wlrSurface) :: Variant 'GodotTy) :: Maybe GodotVariant)
      case maybeWlrSurfaceGV of
          Nothing -> putStrLn "Failed to convert GodotWlrSurface to GodotVariant!"
          Just wlrSurfaceGV -> do G.keyboard_notify_enter wlrSeat wlrSurfaceGV

    -- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
    -- | need one since the GodotWlrSeat keeps internal track of what the currently
    -- | active surface is.
    pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
    pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (ssx, ssy)) = do
      G.pointer_notify_motion wlrSeat ssx ssy
      -- putStrLn $ "G.point_notify_motion: " ++ "(" ++ (show ssx) ++ ", " ++ (show ssy) ++ ")"

    pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
    pointerNotifyButton wlrSeat inputEventType = do
      case inputEventType of
          Motion -> return ()
          Button pressed buttonIndex -> pointerNotifyButton' pressed buttonIndex
      where pointerNotifyButton' pressed buttonIndex = do
              buttonIndexVariant <- toLowLevel (VariantInt buttonIndex) :: IO GodotVariant
              G.pointer_notify_button wlrSeat buttonIndexVariant pressed
              -- putStrLn $ "G.pointer_notify_button: pressed/buttonIndex" ++ (show pressed) ++ "/" ++ (show buttonIndex)
              return ()

    -- | Sends a frame event to the surface with pointer focus (apparently
    -- | useful in particular for axis events); unclear if this is needed in VR but we
    -- | use it regardless.
    pointerNotifyFrame :: GodotWlrSeat -> IO ()
    pointerNotifyFrame wlrSeat = do
      G.pointer_notify_frame wlrSeat

_handle_map :: GFunc GodotSimulaViewSprite
_handle_map self args = do
  putStrLn "_handle_map"
  simulaView <- readTVarIO (self ^. gsvsView)
  G.set_process self True
  G.set_process_input self True -- We do this in Godotston but not in original Simula
  emitSignal self "map" ([self] :: [GodotSimulaViewSprite])
  atomically $ writeTVar (simulaView ^. svMapped) True
  toLowLevel VariantNil

_handle_unmap :: GFunc GodotSimulaViewSprite
_handle_unmap self args = do
  putStrLn "_handle_unmap"
  simulaView <- readTVarIO (self ^. gsvsView)
  atomically $ writeTVar (simulaView ^. svMapped) False
  G.set_process self False
  emitSignal self "unmap" ([self] :: [GodotSimulaViewSprite])
  toLowLevel VariantNil

-- Passes control entirely to updateSimulaViewSprite.
_process :: GFunc GodotSimulaViewSprite
_process self args = do
  case toList args of
    [deltaGV] ->  do
      simulaView <- readTVarIO (self ^. gsvsView)
      mapped <- atomically $ readTVar (simulaView ^. svMapped)
      when mapped $ updateSimulaViewSprite self

  toLowLevel VariantNil


-- The original Simula didn't have a destroy handler at the Godot level.
-- 1. Was it leaking?
-- 2. Do Godot objects get deleted eventually anyway, even if we don't call queue_free?
-- 3. I'm assuming that `registerClass` passes a destructor for
--    GodotSimulaViewSprite that calls to `queue_free` can use.
_handle_destroy :: GFunc GodotSimulaViewSprite
_handle_destroy self args = do
  -- putStrLn "_handle_destroy"
  case toList args of
    [gsvsGV] ->  do
      -- Get state
      maybeGsvs <- variantToReg gsvsGV :: IO (Maybe GodotSimulaViewSprite)
      case maybeGsvs of
        Nothing -> putStrLn "Failed to cast gsvs in _handle_destroy!"
        (Just gsvs) -> do simulaView <- readTVarIO (gsvs ^. gsvsView)
                          gss <- readTVarIO (gsvs ^. gsvsServer)

                          -- Destroy
                          G.queue_free gsvs -- Queue the `gsvs` for destruction
                          G.set_process gsvs False -- Remove the `simulaView ↦ gsvs` mapping from the gss
                          atomically $ modifyTVar' (gss ^. gssViews) (M.delete simulaView)
                            -- Old method of gsvs deletion from Simula; bring back if we face leakage issues:
                            -- Api.godot_object_destroy (safeCast gsvs)

  toLowLevel VariantNil
