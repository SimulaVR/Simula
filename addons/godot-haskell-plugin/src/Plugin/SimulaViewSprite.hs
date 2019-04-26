
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.SimulaViewSprite where

import Debug.C

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Extra.Register
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import qualified Godot.Methods               as G

import Plugin.Types
import Data.Maybe

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

-- | Helper function to convert Haskell timestamps to milliseconds (which wlroots expects) for C marshalling.
toMsec32 :: TimeSpec -> IO (Word32)
toMsec32 timeSpec = do
  let msec = fromIntegral $ toNanoSecs timeSpec `div` 1000000
  return msec

toTimeSpec :: (Integral a) => a -> TimeSpec
toTimeSpec word = (let nsec = (fromIntegral word) * 1000000 in fromNanoSecs nsec)

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

updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  -- Get state
  -- sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  -- tex <- atomically $ readTVar (_gsvsTexture gsvs)
  -- --updateSimulaViewTexture tex

  -- Set extents
  -- sizeChanged gsvs

  -- Tell client it should start drawing next frame.
  -- G.send_frame_done wlrSurface 

  -- whenM (spriteShouldMove gsvs) $ do
  --   atomically $ writeTVar (_gsvsShouldMove gsvs) False
  --   moveToUnoccupied gss gsvs

  -- TODO: Fix this logic to allow for popup drawing.
  -- What we have now: we just paint the toplevel surface onto our sprite.
  -- What we need: recursively draw all of the subsurfaces of the toplevel surface onto our space.
  -- What will probably be useful:j
  --
  --   for_each_surface :: GodotWlrXdgSurface
  --                       -> GodotVariant -- function
  --                       -> IO ()
  --
  -- This 
  return ()
  where drawIndividual sprite wlrSurface = do
          texture <- G.get_texture wlrSurface
          G.set_texture sprite texture

-- Sets extents (meant to be called every frame). Seems poorly named function.
-- Will be called every frame fromUpdateSimulaViewSprite in new Simula.
sizeChanged :: GodotSimulaViewSprite -> IO ()
sizeChanged gsvs = do
  sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  aabb <- G.get_aabb sprite
  size <- godot_aabb_get_size aabb
  shape <- atomically $ readTVar (_gsvsShape gsvs)

  size' <- godot_vector3_operator_divide_scalar size 2

  G.set_extents shape size'

-- Is being called every frame in old Simula; will called from
-- updateSimulaViewSprite in new Simula.
spriteShouldMove :: GodotSimulaViewSprite -> IO Bool
spriteShouldMove gsvs = do
  en <- atomically $ readTVar (_gsvsShouldMove gsvs)
  if en then do
    sprite <- atomically $ readTVar (_gsvsSprite gsvs)
    aabb <- G.get_aabb sprite
    size <- godot_aabb_get_size aabb
    vsize <- fromLowLevel size
    return (vsize > 0)
    else return False

ready :: GFunc GodotSimulaViewSprite
ready self _ = do
  G.set_mode self RigidBody.MODE_KINEMATIC
  toLowLevel VariantNil

inputEvent :: GFunc GodotSimulaViewSprite
inputEvent self args = do
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
  gsvs <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & newNS' []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr :: IO GodotSimulaViewSprite -- w/_gsvsObj populated + mempty TVars

  godotSprite3D <- unsafeInstance GodotSprite3D "Sprite3D"
  G.set_pixel_size godotSprite3D 0.001
  G.add_child gsvs (safeCast godotSprite3D) True
  G.set_flip_h godotSprite3D True

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
  let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
  renderTarget <- initializeRenderTarget wlrXdgSurface
  atomically $ writeTVar (_gsvsViewport gsvs) renderTarget

  updateSimulaViewSprite gsvs -- Now we update everything

  return gsvs

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  -- Get state:
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView) 
  let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
  wlrSurface  <- G.get_wlr_surface wlrXdgSurface
  wlrSurface' <- asGodotVariant wlrSurface
  toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer) -- ^. gssWlrSeat)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)

  -- Make calls:
  G.set_activated toplevel True
  G.keyboard_notify_enter wlrSeat wlrSurface'

  return ()


-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
-- |
-- | TODO: Change this horifically named function.
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> GodotVector3
                  -> IO ()
processClickEvent gsvs evt clickPos = do
  -- Get state
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let godotWlrXdgSurface = (simulaView ^. svWlrXdgSurface)


  -- Compute subsurface local coordinates at clickPos
  surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) <- getSurfaceLocalCoordinates
  (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) <- getSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords

  -- -- Send events
  case evt of
    Motion                -> do pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                                pointerNotifyMotion wlrSeat subSurfaceLocalCoords
    Button pressed button ->    pointerNotifyButton wlrSeat evt

  pointerNotifyFrame wlrSeat -- No matter what, send a frame event to the surface with pointer focus
                             -- TODO: Is this necessary in VR?

  where
    getSurfaceLocalCoordinates :: IO (SurfaceLocalCoordinates)
    getSurfaceLocalCoordinates = do
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
      let sx = fromIntegral $ truncate (256 * coords ^. _x)
          sy = fromIntegral $ truncate (256 * coords ^. _y)
      return (SurfaceLocalCoordinates (sx, sy))

    -- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
    getSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
    getSubsurfaceAndCoords wlrXdgSurface (SurfaceLocalCoordinates (sx, sy)) = do
      wlrSurfaceAtResult   <- G.surface_at  wlrXdgSurface sx sy
      wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult
      ssx                  <- G.get_sub_x wlrSurfaceAtResult
      ssy                  <- G.get_sub_y wlrSurfaceAtResult
      let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
      return (wlrSurfaceSubSurface, ssCoordinates)

    -- | Let wlroots know we have entered a new surface. We can safely call this
    -- | over and over (wlroots checks if we've called it already for this surface
    -- | and, if so, returns early.
    pointerNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
    pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (ssx, ssy)) = do
      let maybeWlrSurfaceGV = (fromVariant ((toVariant wlrSurface) :: Variant 'GodotTy) :: Maybe GodotVariant)
      case maybeWlrSurfaceGV of
          Nothing -> putStrLn "Failed to convert GodotWlrSurface to GodotVariant!"
          Just wlrSurfaceGV -> G.pointer_notify_enter wlrSeat wlrSurfaceGV ssx ssy

    -- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
    -- | need one since the GodotWlrSeat keeps internal track of what the currently
    -- | active surface is.
    pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
    pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (ssx, ssy)) = do
      G.pointer_notify_motion wlrSeat ssx ssy

    pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
    pointerNotifyButton wlrSeat inputEventType = do
      case inputEventType of
          Motion -> return ()
          Button pressed buttonIndex -> pointerNotifyButton' pressed buttonIndex
      where pointerNotifyButton' pressed buttonIndex = do
              buttonIndexVariant <- toLowLevel (VariantInt buttonIndex) :: IO GodotVariant
              G.pointer_notify_button wlrSeat buttonIndexVariant pressed
              return ()

    -- | Sends a frame event to the surface with pointer focus (apparently
    -- | useful in particular for axis events); unclear if this is needed in VR but we
    -- | use it regardless.
    pointerNotifyFrame :: GodotWlrSeat -> IO ()
    pointerNotifyFrame wlrSeat = do
      G.pointer_notify_frame wlrSeat

_handle_map :: GFunc GodotSimulaViewSprite
_handle_map self args = do
  case toList args of
    [gsvsGV] ->  do
      G.set_process self True
      -- G.set_process_input self True -- We do this in Godotston but not in original Simula; deciding to not do it

      -- Am mirroring godotston here, but conceptually it's unclear to me what
      -- this is doing, and why it wouldn't just result in a _handle_map call
      -- loop that never terminates.
      emitSignal self "map" ([] :: [GodotSimulaViewSprite])
      putStrLn "Called _handle_map. Should only see this once per surface launch." -- <- Test

  toLowLevel VariantNil

_handle_unmap :: GFunc GodotSimulaViewSprite
_handle_unmap self args = do
  case toList args of
    [gsvsGV] ->  do
      maybeGsvs <- variantToReg gsvsGV :: IO (Maybe GodotSimulaViewSprite)
      case maybeGsvs of
        Nothing -> putStrLn "Failed to cast gsvs in _handle_destroy!"
        (Just gsvs) -> G.set_process gsvs False
                      -- G.set_process_input gsvs False -- We do this in Godotston but not in original Simula; deciding to not do it

      -- Am mirroring godotston here, but conceptually it's unclear to me what
      -- this is doing, and why it wouldn't just result in a _handle_unmap call
      -- loop that never terminates.
      emitSignal self "unmap" ([] :: [GodotSimulaViewSprite])
      putStrLn "Called _handle_unmap. Should only see this once per surface launch." -- <- Test
  toLowLevel VariantNil

-- Passes control entirely to updateSimulaViewSprite.
_process :: GFunc GodotSimulaViewSprite
_process self args = do
  case toList args of
    [deltaGV] ->  do
      updateSimulaViewSprite self

  toLowLevel VariantNil


-- The original Simula didn't have a destroy handler at the Godot level.
-- 1. Was it leaking?
-- 2. Do Godot objects get deleted eventually anyway, even if we don't call queue_free?
-- 3. I'm assuming that `registerClass` passes a destructor for
--    GodotSimulaViewSprite that calls to `queue_free` can use.
_handle_destroy :: GFunc GodotSimulaViewSprite
_handle_destroy self args = do
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

 -- Our overall rendering strategy: each GodotSimulaViewSprite has a
 -- GodotSprite3D, which expects a texture with our wlroots surface. In order to
 -- supply this wlroots texture, we have to make it.

 -- To do this, we start with the texture provided by our GodotWlrXdgSurface,
 -- and apply it it to a Viewport (used purely as a "rendering target") as a
 -- base, and then render each of the GodotWlrXdgSurface's subsurfaces over it
 -- (which contain popups, menus, etc). The final result will be a Viewport
 -- texture with all of the subsurfaces pancaked on top of each other, creating
 -- the illusion of one large wlroots surface. We finish by supplying this
 -- Viewport texture to our Sprite3D, creating the desired illusion of having a
 -- wlroots surface floating in 3-space.

 -- NOTE: You might ask, "why must we use a Viewport?" The answer is that this
 -- is the ritual we engage in to modify 2D textures in a 3D Godot game. As long
 -- as our Viewport isn't the Root Viewport, it is treated by Godot as a
 -- "rendering target" instead of a screen to render onto. For this reason we
 -- associate with each GodotSimulaViewSprite its own unique GodotViewport.

-- | Draws the given surface onto the GodotSimulaViewSprite's Viewport (treated
-- | as a rendering target). Meant to be called for all of the
-- | GodotSimulaViewSprite's subsurfaces, to create the texture to be applied for
-- | the GodotSimulaViewSprite's Sprite3D.
-- | 
-- | NOTE: This function is intended to be used as a closure over its first
-- |       argument and fed to for_each_ffi as a FunPtr.
drawSurface :: GodotSimulaViewSprite
            -> GodotWlrSurface
            -> CInt
            -> CInt
            -> IO ()
drawSurface gsvs wlrSurface sx sy = do
  -- Get state
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
  renderPosition <- getCoordinatesFromCenter wlrXdgSurface sx sy -- Surface coordinates are relative to the size of the GodotWlrXdgSurface
  textureToDraw <- G.get_texture wlrSurface :: IO GodotTexture
  renderTarget <- readTVarIO (gsvs ^. gsvsViewport)

    -- Send draw command
  godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
  let nullTexture = Data.Maybe.fromJust ((fromVariant VariantNil) :: Maybe GodotTexture) :: GodotTexture
  -- G.draw_texture ((safeCast renderTarget) :: GodotCanvasItem) textureToDraw renderPosition godotColor nullTexture

  -- Tell the surface being drawn it can start to render its next frame
  G.send_frame_done wlrSurface
  -- NOTE: I'm omitting re-mutating gsvsViewport, G.draw_texture should merely
  -- mutate what it's pointing to anyway.
  where
        -- | Convert (sx,sy) coordinates from "top-left" to "from-center" coordinate systems.
        getCoordinatesFromCenter :: GodotWlrXdgSurface -> CInt -> CInt -> IO GodotVector2
        getCoordinatesFromCenter wlrXdgSurface sx sy = do
          (bufferWidth', bufferHeight')    <- getBufferDimensions wlrXdgSurface
          let (bufferWidth, bufferHeight)  = (fromIntegral bufferWidth', fromIntegral bufferHeight')
          let (fromTopLeftX, fromTopLeftY) = (fromIntegral sx, fromIntegral sy)
          let fromCenterX                  = -(bufferWidth/2) + fromTopLeftX
          let fromCenterY                  = -(-(bufferHeight/2) + fromTopLeftY)
          -- NOTE: In godotston fromCenterY is isn't negative, but since we set
          -- `G.render_target_v_flip viewport True` we can set this
          -- appropriately
          -- NOTE: We above assume that
          --    G.render_target_v_flip viewport True
          -- has been set.
          let v2 = (V2 fromCenterX fromCenterY) :: V2 Float
          gv2 <- toLowLevel v2 :: IO GodotVector2
          return gv2

-- Leaving up for a day in case I need it; delete tomorrow.
--
-- _draw_surface :: GFunc GodotSimulaViewSprite
-- _draw_surface self args = do
--   case toList args of
--     [gsvsGV, sxGV, syGV] -> do
--       -- Get state
--       maybeGsvs <- variantToReg gsvsGV
--       sxV <- fromLowLevel sxGV
--       sxY <- fromLowLevel syGV
--       case (maybeGsvs, sxV, sxY) of
--         (Just gsvs, (VariantInt sx), (VariantInt sy)) -> do
--            (fromCenterX, fromCenterY) <- getCoordinatesFromCenter gsvs sx sy

--             -- Send draw command let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
--             renderTarget <- readTVarIO (gsvs ^. gsvsViewport)
--             godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
--             let nullTexture = Data.Maybe.fromJust ((fromVariant VariantNil) :: Maybe GodotTexture) :: GodotTexture
--            -- G.draw_texture renderTarget ..
--            -- G.send_frame_done wlrSurface
--            -- mutate GodotViewport
--       retnil

--            -- Draw subsurface onto Viewport
--         _ -> do putStrLn "Unable to cast types in _draw_surface!"
--                 retnil
--      where getCoordinatesFromCenter :: GodotSimulaViewSprite -> Int -> Int -> GodotVector2
--            getCoordinatesFromCenter gsvs sx sy = do
--             -- Get more state
--             simulaView            <- readTVarIO (gsvs ^. gsvsView)
--             let wlrXdgSurface     = (simulaView ^. svWlrXdgSurface)

--             -- Convert (sx,sy) coordinates from "top-left" to "from-center" coordinate systems.
--             (bufferWidth', bufferHeight')      <- getBufferDimensions wlrXdgSurface
--             let (bufferWidth, bufferHeight)  = (fromIntegral bufferWidth', fromIntegral bufferHeight')
--             let (fromTopLeftX, fromTopLeftY) = (fromIntegral sx, fromIntegral sy)
--             let fromCenterX                  = -(bufferWidth/2) + fromTopLeftX
--             let fromCenterY                  = -(-(bufferHeight/2) + fromTopLeftY)
--             -- NOTE: In godotston fromCenterY is isn't negative, but since we set
--             -- `G.render_target_v_flip viewport True` we can set this
--             -- appropriately
--             -- NOTE: We above assume that
--             --    G.render_target_v_flip viewport True
--             -- has been set.
--             let v2 = (V2 fromCenterX fromCenterY) :: V2 Float
--             gv2 <- toLowLevel v2 :: IO GodotVector2
--             return gv2


initializeRenderTarget :: GodotWlrXdgSurface -> IO (GodotViewport)
initializeRenderTarget wlrXdgSurface = do
  -- "When we are drawing to a Viewport that is not the Root, we call it a
  --  render target." -- Godot documentation"
  renderTarget <- unsafeInstance GodotViewport "Viewport"
  -- No need to add the Viewport to the SceneGraph since we plan to use it as a render target
    -- G.set_name viewport =<< toLowLevel "Viewport"
    -- G.add_child gsvs ((safeCast viewport) :: GodotObject) True

  G.set_disable_input renderTarget True -- Turns off input handling

  G.set_usage renderTarget 0 -- USAGE_2D = 0
  -- G.set_hdr renderTarget False -- Might be useful to disable HDR rendering for performance in the future

  -- CLEAR_MODE_ALWAYS = 0
  -- CLEAR_MODE_NEVER = 1
  -- I think we need CLEAR_MODE_ALWAYS since, i.e., a popup dragging might cause a trail?
  G.set_clear_mode renderTarget 0

  -- Perhaps we should never update the render target, since we do so manually each frame?
  -- UPDATE_DISABLED = 0 — Do not update the render target.
  -- UPDATE_ONCE = 1 — Update the render target once, then switch to UPDATE_DISABLED.
  -- UPDATE_WHEN_VISIBLE = 2 — Update the render target only when it is visible. This is the default value.
  -- UPDATE_ALWAYS = 3 — Always update the render target. 
  G.set_update_mode renderTarget 3 -- Using UPDATE_ALWAYS for now

  -- "Note that due to the way OpenGL works, the resulting ViewportTexture is flipped vertically. You can use Image.flip_y on the result of Texture.get_data to flip it back[or you can also use set_vflip]:" -- Godot documentation
  G.set_vflip renderTarget True -- In tutorials this is set as True, but no reference to it in Godotston; will set to True for now

  -- We could alternatively set the size of the renderTarget via set_size_override [and set_size_override_stretch]
  dimensions@(width, height) <- getBufferDimensions wlrXdgSurface
  pixelDimensionsOfWlrXdgSurface <- toGodotVector2 dimensions

  -- Here I'm attempting to set the size of the viewport to the pixel dimensions
  -- of our wlrXdgSurface argument:
  G.set_size renderTarget pixelDimensionsOfWlrXdgSurface

  -- There is, however, an additional way to do this and I'm not sure which one
  -- is better/more idiomatic:
    -- G.set_size_override renderTarget True vector2
    -- G.set_size_override_stretch renderTarget True

  return renderTarget
  where
        -- | Used to supply GodotVector2 to
        -- |   G.set_size :: GodotViewport -> GodotVector2 -> IO ()
        toGodotVector2 :: (Int, Int) -> IO (GodotVector2)
        toGodotVector2 (width, height) = do
          let v2 = (V2 (fromIntegral width) (fromIntegral height))
          gv2 <- toLowLevel v2 :: IO (GodotVector2)
          return gv2

-- Possible TODO: Place types for different coordinate systems
-- newtype BufferDimensions = BufferDimensions (Int, Int)
-- data FromTopLeftCoordinates = FromTopLeftCoordinates (Int, Int)
-- data FromCenterCoordinates = FromCenterCoordinates (Int, Int)

getBufferDimensions :: GodotWlrXdgSurface -> IO (Int, Int)
getBufferDimensions wlrXdgSurface = do
  wlrSurface <- G.get_wlr_surface wlrXdgSurface
  wlrSurfaceState <- G.get_current_state wlrSurface
  bufferWidth <- G.get_buffer_width wlrSurfaceState
  bufferHeight <- G.get_buffer_height wlrSurfaceState
  -- width <- G.get_width wlrSurfaceState
  -- height <-G.get_height wlrSurfaceState
  return (bufferWidth, bufferHeight) -- G.set_size expects "the width and height of viewport" according to Godot documentation

getTextureFromRenderTarget :: GodotViewport -> IO (GodotTexture)
getTextureFromRenderTarget renderTarget = do
    viewportTexture' <- G.get_texture renderTarget -- G.get_texture :: GodotViewport -> (IO GodotViewportTexture)
    let viewportTexture = (safeCast viewportTexture) :: GodotTexture -- GodotTexture :< GodotViewportTexture
    -- -- Retrieving an image
    -- G.get_data viewportTexture :: IO GodotImage -- requires viewportTexture to be cast as GodotTexture
    return viewportTexture

