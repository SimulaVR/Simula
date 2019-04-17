
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
  classExtends = "RigidBody"
  classMethods =
    [ GodotMethod NoRPC "_input_event" inputEvent
    , GodotMethod NoRPC "_ready" ready
    ]

  -- Test:
  classSignals = [ signal "map" [("gsvs", GodotVariantTypeObject)]
                 , signal "unmap" [("gsvs", GodotVariantTypeObject)]
                 ]

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ _ ) = GodotRigidBody obj

updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  -- sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  -- tex <- atomically $ readTVar (_gsvsTexture gsvs)
  -- updateSimulaViewTexture tex
  -- G.set_texture sprite (safeCast tex)
  -- sizeChanged gsvs
  return ()

-- Seems poorly named function.
sizeChanged :: GodotSimulaViewSprite -> IO ()
sizeChanged gsvs = do
  sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  aabb <- G.get_aabb sprite
  size <- godot_aabb_get_size aabb
  shape <- atomically $ readTVar (_gsvsShape gsvs)

  size' <- godot_vector3_operator_divide_scalar size 2

  G.set_extents shape size'

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

  updateSimulaViewSprite gsvs -- Now we update everything

  return gsvs

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  -- Get state:
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView) 
  let wlrXdgSurface = (simulaView ^. gsvsWlrXdgSurface)
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
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> GodotVector3
                  -> IO ()
processClickEvent gsvs evt clickPos = do
  -- Get state
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  return ()

  -- maybeSubSurfaceData <- viewAt simulaView (SurfaceLocalCoordinates (sx, sy))
  -- case (maybeSubSurfaceData, evt) of
  --   (Nothing, _) -> return ()
  --   (Just (subSurfaceAtPoint, SubSurfaceLocalCoordinates (ssx, ssy)), Motion)                -> processMouseMotionEvent seat subSurfaceAtPoint ssx ssy
  --   (Just (subSurfaceAtPoint, SubSurfaceLocalCoordinates (ssx, ssy)), Button pressed button) -> processMouseButtonEvent seat simulaView subSurfaceAtPoint ssx ssy pressed button
  -- where
  --   getSurfaceLocalCoordinates gsvs _ clickPos = do
  --     lpos <- G.to_local gsvs clickPos >>= fromLowLevel
  --     sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  --     aabb <- G.get_aabb sprite
  --     size <- godot_aabb_get_size aabb >>= fromLowLevel
  --     let topleftPos =
  --           V2 (size ^. _x / 2 - lpos ^. _x) (size ^. _y / 2 - lpos ^. _y)
  --     let scaledPos = liftI2 (/) topleftPos (size ^. _xy)
  --     rect <- G.get_item_rect sprite
  --     recSize <- godot_rect2_get_size rect >>= fromLowLevel
  --     let coords = liftI2 (*) recSize scaledPos
  --     -- coords = surface coordinates in pixel with (0,0) at top left
  --     let sx = fromIntegral $ truncate (256 * coords ^. _x)
  --         sy = fromIntegral $ truncate (256 * coords ^. _y)
  --     return (SurfaceLocalCoordinates (sx, sy))
  --  -- See https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
  --   toInputEventCode :: Int -> Word32
  --   toInputEventCode BUTTON_LEFT = 0x110 -- BTN_LEFT
  --   toInputEventCode BUTTON_RIGHT = 0x111 -- BTN_RIGHT
  --   toInputEventCode BUTTON_MIDDLE = 0x112 -- BTN_MIDDLE
  --   toInputEventCode BUTTON_WHEEL_DOWN = 0x150 -- BTN_GEAR_DOWN
  --   toInputEventCode BUTTON_WHEEL_UP = 0x151 -- BTN_GEAR_UP
  --   toInputEventCode _ = 0x110
  --   toButtonState True = ButtonPressed
  --   toButtonState False = ButtonReleased
  --   getSeatFocusedSurface seat = do
  --     let seat' = toInlineC seat
  --     lastFocusedSurface' <-
  --       [C.exp| struct wlr_surface * { $(struct wlr_seat * seat')->pointer_state.focused_surface } |] -- hsroots doesn't provide access to this data structure AFAIK
  --     return $ toC2HS lastFocusedSurface'
  --   getNow32 = do
  --     nowTimeSpec <- (getTime Realtime)
  --     now32 <- toMsec32 nowTimeSpec
  --     return now32
  --   processMouseMotionEvent seat subSurfaceAtPoint ssx ssy = do
  --     now32 <- getNow32
  --     seatFocusedSurface <- getSeatFocusedSurface seat
  --     let isNewSurface = (seatFocusedSurface /= subSurfaceAtPoint)
  --     -- Core calls:
  --     when isNewSurface       $ do pointerNotifyEnter seat subSurfaceAtPoint ssx ssy
  --     when (not isNewSurface) $ do pointerNotifyMotion seat now32 ssx ssy
  --   processMouseButtonEvent seat simulaView subSurfaceAtPoint ssx ssy pressed button = do
  --     now32 <- getNow32
  --     let button' = (toInputEventCode button)   :: Word32
  --     let buttonState = (toButtonState pressed) :: ButtonState
  --     -- Core calls:
  --     pointerNotifyButton seat now32 button' buttonState -- Notify the client with pointer focus that a button press (or release) has occurred; I'm assuming the surface coordinates are obtained internally
  --     case buttonState of
  --       ButtonReleased -> putStrLn "button released" -- Here would be a good time to adjust SimulaCursorMode state back to pass-through in the future
  --       ButtonPressed -> do
  --         putStrLn "button pressed"
  --         focusView simulaView subSurfaceAtPoint -- Ensure the keyboard has focus if there's a view at point


-- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
getSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
getSubsurfaceAndCoords wlrXdgSurface (SurfaceLocalCoordinates (sx, sy)) = do
  wlrSurfaceAtResult   <- G.surface_at  wlrXdgSurface sx sy
  wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult
  ssx                  <- G.get_sub_x wlrSurfaceAtResult
  ssy                  <- G.get_sub_y wlrSurfaceAtResult
  let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
  return (wlrSurfaceSubSurface, ssCoordinates)

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

-- | gdwlroots input functions dictionary
{-
G.get_surface :: GodotWlrSurfaceAtResult -> (IO GodotWlrSurface)

G.get_sub_x :: GodotWlrSurfaceAtResult
            -> (IO Float) -- ssx
G.get_sub_y :: GodotWlrSurfaceAtResult
            -> (IO Float) -- ssy

G.surface_at :: GodotWlrXdgSurface
             -> Float -- sx
             -> Float -- sy
             -> IO (GodotWlrSurfaceAtResult) -- i.e. xdg subsurface at local surface coords

G.pointer_notify_enter :: GodotWlrSeat
                       -> GodotVariant -- WlrSurface (I'm assuming "subsurface" in our lingo) casted as GodotVariant
                       -> Float        -- ssx
                       -> Float        -- ssy
                       -> IO ()

G.pointer_notify_motion :: GodotWlrSeat
                        -> Float        -- ssx
                        -> Float        -- ssy
                           IO ()

G.pointer_notify_button :: GodotWlrSeat
                        -> GodotVariant -- event.button_index (need to go Int -> GodotVariant)
                        -> Bool         -- event.pressed
                        -> IO Int       -- Sort sort exit code?
-}