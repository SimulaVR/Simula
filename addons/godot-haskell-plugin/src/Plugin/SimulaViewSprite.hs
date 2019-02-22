{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Plugin.SimulaViewTexture

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Control.Lens                hiding (Context)

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

-- | We will say that two views are "the same" when they have the same Ptr WlrXdgSurface (this is
-- | is a bad idea long-term but for now will suffice; for example, we might have two
-- | distinct views [one used as an icon and one a window] that have the same underlying surface).
-- | TODO: Give SimulaView's a unique id and change this function accordingly.
isSameView :: SimulaView -> SimulaView -> Bool
isSameView simulaView1 simulaView2 = (_svXdgSurface simulaView1) == (_svXdgSurface simulaView2)

-- | This function takes a pair of surface coordinates (against a given view) and
-- | inspects if there are any XDG toplevel "subsurfaces" at those coordinates (this
-- | could either be a true "subsurface" -- like a popup -- or the trivial subsurface
-- | [the parent surface associated with the view itself]). We return the wlr_surface (if there is one)
-- | associated with the "subsurface", and convert the surface coordinates to the
-- | corresponding *subsurface* coordinates (relative to the output subsurface).
viewAt :: SimulaView -> SurfaceLocalCoordinates -> IO (Maybe (Ptr WlrSurface, SubSurfaceLocalCoordinates))
viewAt simulaView (SurfaceLocalCoordinates (sx, sy)) = do
  let xdgSurface = (simulaView ^. svXdgSurface)
  maybeSubSurface <- xdgSurfaceAt xdgSurface sx sy
  case maybeSubSurface of
       Nothing                     -> return $ Nothing
       Just (subSurface, ssx, ssy) -> return $ Just (subSurface, SubSurfaceLocalCoordinates (ssx, ssy))

-- This function takes a SimulaView to (i) defocus the keyboard's old surface and (ii)
-- focus on the new SimulaView. (To understand this function, mostly ignore the second
-- argument).
--
-- When a view is "defocused" a call to `wlr_xdg_toplevel_set_activated` is made
-- (w/false flag). When a view is "focused" it is (i) called with
-- wlr_xdg_toplevel_set_activated (w/true flag); (ii) called with
-- wlr_seat_keyboard_notify_enter.
--
-- NOTE: It's unclear why this function doesn't just take a SimulaView, and then
-- use that to extract a Ptr WlrSurface. For now I'll keep the second argument
-- (which seems redundant) to mirror the C implementation.
focusView :: SimulaView -> Ptr WlrSurface -> IO ()
focusView simulaView ptrWlrSurface = do
  let ptrWlrSeat =_gssSeat (_svServer simulaView)
  prevSurface <- getKeyboardFocus (getKeyboardState ptrWlrSeat)
  when (ptrWlrSurface /= prevSurface) $ do when (prevSurface /= nullPtr) $ deactivatePreviouslyFocusedSurface prevSurface
                                           -- mutateViewToFront simulaView
                                           setActivated (_svXdgSurface simulaView) True
                                           keyboardNotifyEnterIntoNewSurface ptrWlrSeat (_svXdgSurface simulaView)
  where
        -- |Let's client know it no longer has focus (so it can, i.e., stop displaying caret).
        deactivatePreviouslyFocusedSurface prevSurface = do
            let prevSurface' = (toInlineC prevSurface) :: Ptr C'WlrSurface
            prevSurfaceXdg <- [C.exp| struct wlr_xdg_surface * { wlr_xdg_surface_from_wlr_surface( $(struct wlr_surface * prevSurface') ) }|] -- hsroots lacks this function so we use inline-C
            setActivated (toC2HS prevSurfaceXdg) False
        keyboardNotifyEnterIntoNewSurface ptrWlrSeat viewXdgSurface = do
          maybePtrWlrKeyboard          <- getSeatKeyboard ptrWlrSeat
          maybeXdgSurfaceAssociatedSurface <- xdgSurfaceGetSurface (_svXdgSurface simulaView)
          case (maybePtrWlrKeyboard, maybeXdgSurfaceAssociatedSurface) of
              (Nothing, _)               -> putStrLn "Couldn't get keyboard!"
              (_, Nothing)               -> putStrLn "Couldn't get surface!"
              (Just ptrWlrKeyboard, Just surface) -> do
                (keycodes, numKeycodes) <- getKeyboardKeys ptrWlrKeyboard
                let modifiers = getModifierPtr ptrWlrKeyboard
                keyboardNotifyEnter ptrWlrSeat surface keycodes numKeycodes modifiers

---------------

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance GodotClass GodotSimulaViewSprite where
  godotClassName = "SimulaViewSprite"

instance ClassExport GodotSimulaViewSprite where
  classInit obj =
    GodotSimulaViewSprite obj
                  <$> atomically (newTVar True)
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite.")) 
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite.")) 
  classExtends = "RigidBody"
  classMethods =
    [ GodotMethod NoRPC "_input_event" inputEvent
    , GodotMethod NoRPC "_ready" ready
    ]

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ ) = GodotRigidBody obj

newGodotSimulaViewSprite :: SimulaView -> GodotSimulaViewTexture -> IO GodotSimulaViewSprite
newGodotSimulaViewSprite simulaView tex = do
  gsvs <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & unsafeNewNS id "Object" []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr

  sprite <- unsafeInstance GodotSprite3D "Sprite3D"
  G.set_pixel_size sprite 0.001
  G.add_child gsvs (safeCast sprite) True
  G.set_flip_h sprite True

  shape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gsvs (safeCast gsvs)
  G.shape_owner_add_shape gsvs ownerId (safeCast shape)

  -- We don't need to fill gsvsObj or gsvsShouldMove (already set via classInit)
  atomically $ writeTVar (_gsvsSprite  gsvs ) sprite
  atomically $ writeTVar (_gsvsShape   gsvs ) shape
  atomically $ writeTVar (_gsvsTexture gsvs ) tex
  return gsvs

updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  tex <- atomically $ readTVar (_gsvsTexture gsvs)
  updateSimulaViewTexture tex
  G.set_texture sprite (safeCast tex)
  sizeChanged gsvs

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


-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
processClickEvent :: GodotSimulaViewSprite -> InputEventType -> GodotVector3 -> IO ()
processClickEvent gsvs evt clickPos = do
  texture <- atomically $ readTVar (gsvs ^. gsvsTexture)
  simulaView <- atomically $ readTVar (texture ^. gsvtView)
  let seat = simulaView ^. svServer ^. gssSeat

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
  maybeSubSurfaceData <- viewAt simulaView (SurfaceLocalCoordinates (sx, sy))
  case (maybeSubSurfaceData, evt) of
    (Nothing, _) -> return ()
    (Just (subSurfaceAtPoint, SubSurfaceLocalCoordinates (ssx, ssy)), Motion)                -> processMouseMotionEvent seat subSurfaceAtPoint ssx ssy
    (Just (subSurfaceAtPoint, SubSurfaceLocalCoordinates (ssx, ssy)), Button pressed button) -> processMouseButtonEvent seat simulaView subSurfaceAtPoint ssx ssy pressed button
  where
   -- See https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
    toInputEventCode :: Int -> Word32
    toInputEventCode BUTTON_LEFT = 0x110 -- BTN_LEFT
    toInputEventCode BUTTON_RIGHT = 0x111 -- BTN_RIGHT
    toInputEventCode BUTTON_MIDDLE = 0x112 -- BTN_MIDDLE
    toInputEventCode BUTTON_WHEEL_DOWN = 0x150 -- BTN_GEAR_DOWN
    toInputEventCode BUTTON_WHEEL_UP = 0x151 -- BTN_GEAR_UP
    toInputEventCode _ = 0x110
    toButtonState True = ButtonPressed
    toButtonState False = ButtonReleased
    getSeatFocusedSurface seat = do
      let seat' = toInlineC seat
      lastFocusedSurface' <-
        [C.exp| struct wlr_surface * { $(struct wlr_seat * seat')->pointer_state.focused_surface } |] -- hsroots doesn't provide access to this data structure AFAIK
      return $ toC2HS lastFocusedSurface'
    getNow32 = do
      nowTimeSpec <- (getTime Realtime)
      now32 <- toMsec32 nowTimeSpec
      return now32
    processMouseMotionEvent seat subSurfaceAtPoint ssx ssy = do
      now32 <- getNow32
      seatFocusedSurface <- getSeatFocusedSurface seat
      let isNewSurface = (seatFocusedSurface /= subSurfaceAtPoint)
      -- Core calls:
      when isNewSurface       $ do pointerNotifyEnter seat subSurfaceAtPoint ssx ssy
      when (not isNewSurface) $ do pointerNotifyMotion seat now32 ssx ssy
    processMouseButtonEvent seat simulaView subSurfaceAtPoint ssx ssy pressed button = do
      now32 <- getNow32
      let button' = (toInputEventCode button)   :: Word32
      let buttonState = (toButtonState pressed) :: ButtonState
      -- Core calls:
      pointerNotifyButton seat now32 button' buttonState -- Notify the client with pointer focus that a button press (or release) has occurred; I'm assuming the surface coordinates are obtained internally
      case buttonState of
        ButtonReleased -> putStrLn "button released" -- Here would be a good time to adjust SimulaCursorMode state back to pass-through in the future
        ButtonPressed -> do
          putStrLn "button pressed"
          focusView simulaView subSurfaceAtPoint -- Ensure the keyboard has focus if there's a view at point
