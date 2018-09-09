{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
module Plugin.Weston where

import Simula.WaylandServer
import Simula.Weston
import Simula.WestonDesktop

import Data.Coerce

import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T
import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import           Godot.Gdnative.Types        (GodotFFI, LibType, TypeOf)
import qualified Godot.Methods               as G

import qualified Data.Map.Strict as M
import Plugin.WestonSurfaceSprite
import Plugin.WestonSurfaceTexture
import Plugin.SimulaController

import Godot.Core.GodotGlobalConstants


import Control.Monad
import Control.Concurrent
import System.Environment

import System.Posix.Signals

import Data.Bits
import qualified Data.Vector as V

import Control.Lens

import Foreign hiding (void)

import Telemetry

data GrabState
  = NoGrab
  | Manipulating (GodotSimulaController, GodotWestonSurfaceSprite)
  | ManipulatingDual
      (GodotSimulaController, GodotWestonSurfaceSprite)
      (GodotSimulaController, GodotWestonSurfaceSprite)
  | Resizing (GodotSimulaController, GodotSimulaController) GodotWestonSurfaceSprite Float -- dist

data GodotWestonCompositor = GodotWestonCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar WestonCompositor
  , _gwcWlDisplay :: TVar WlDisplay
  , _gwcSurfaces :: TVar (M.Map WestonSurface GodotWestonSurfaceSprite)
  , _gwcOutput :: TVar WestonOutput
  , _gwcNormalLayer :: TVar WestonLayer
  , _gwcGrabState :: TVar GrabState
  }

instance GodotClass GodotWestonCompositor where
  godotClassName = "WestonCompositor"

instance ClassExport GodotWestonCompositor where
  classInit obj  = GodotWestonCompositor obj <$> atomically (newTVar undefined) <*> atomically (newTVar undefined) <*> atomically (newTVar mempty) <*> atomically (newTVar undefined)
                   <*> atomically (newTVar undefined) <*> atomically (newTVar NoGrab)
    
  classExtends = "Spatial"
  classMethods = [ Func NoRPC "_ready" startBaseCompositor
                 , Func NoRPC "_input" input
                 , Func NoRPC "_process" process
                 , Func NoRPC "on_button_signal" on_button_signal ]

startBaseCompositor :: GodotFunc GodotWestonCompositor
startBaseCompositor _ compositor _ = do
  onReady compositor

  getCamera (safeCast compositor) >>= \case
    Just hmd -> do
      let comp = safeCast compositor :: GodotSpatial
      -- | Position in front of camera
      -- FIXME: Messes with moveToUnoccupied and makes windows stack
      posInfront hmd comp

      -- | Face the camera
      faceCamera hmd comp
    Nothing -> return ()

  startBaseThread compositor

  startTelemetry (_gwcSurfaces compositor)

  toLowLevel VariantNil
 where
  posInfront :: GodotARVRCamera -> GodotSpatial -> IO ()
  posInfront cam comp = do
    TF b _ <- G.get_global_transform (comp `as` GodotSpatial) >>= fromLowLevel
    TF camBasis camPos <- G.get_global_transform cam >>= fromLowLevel
    let dist = 1 -- ^ Distance from camera
        fw = negate $ camBasis ^. _z
        newPos = camPos + fw ^* dist + V3 0 0.5 0
    tf <- toLowLevel (TF b newPos) :: IO GodotTransform
    G.set_global_transform comp tf

  faceCamera :: GodotARVRCamera -> GodotSpatial -> IO ()
  faceCamera hmd spatial = do
    camPos <- G.get_global_transform (safeCast hmd :: GodotSpatial)
      >>= Api.godot_transform_get_origin
    spatialOrig <- G.get_global_transform spatial
      >>= Api.godot_transform_get_origin
    whenM (not <$> isSameHoriz spatialOrig camPos)
      $ toLowLevel (V3 0 1 0) >>= spatial `G.look_at` camPos

  isSameHoriz :: GodotVector3 -> GodotVector3 -> IO Bool
  isSameHoriz a b = do
    (V3 x _ z) <- fromLowLevel a :: IO (V3 Float)
    (V3 x' _ z') <- fromLowLevel b :: IO (V3 Float)
    return $ (V2 x z) == (V2 x' z')

  getCamera :: GodotNode -> IO (Maybe GodotARVRCamera)
  getCamera nd = nd `getNode` "../ARVROrigin/ARVRCamera" >>= \case
    Just cam -> return $ Just $ GodotARVRCamera $ safeCast cam
    Nothing -> return Nothing

onReady :: GodotWestonCompositor -> IO ()
onReady gwc = do
  leftCt <- toLowLevel "../ARVROrigin/LeftController" >>= G.get_node gwc
  rightCt <- toLowLevel "../ARVROrigin/RightController" >>= G.get_node gwc

  connectController leftCt
  connectController rightCt
  where
    connectController ct = do
      btnPressed <- toLowLevel "button_pressed"
      btnReleased <- toLowLevel "button_release"
      btnSignal <- toLowLevel "on_button_signal"

      argsPressed <- Api.godot_array_new 
      toLowLevel (toVariant ct) >>= Api.godot_array_append  argsPressed
      toLowLevel (toVariant True) >>= Api.godot_array_append argsPressed
      
      argsReleased <- Api.godot_array_new 
      toLowLevel (toVariant ct) >>= Api.godot_array_append  argsReleased
      toLowLevel (toVariant False) >>= Api.godot_array_append argsReleased

      G.connect ct btnPressed (safeCast gwc) btnSignal argsPressed 0
      G.connect ct btnReleased (safeCast gwc) btnSignal argsReleased 0
      return ()

startBaseThread :: GodotWestonCompositor -> IO ()
startBaseThread compositor = void $ forkOS $ do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr
  atomically $ writeTVar (_gwcCompositor compositor) wcomp
  atomically $ writeTVar (_gwcWlDisplay compositor) wldp
  westonCompositorSetRepaintMsec wcomp 1000

  setup_weston_log_handler
  westonCompositorSetEmptyRuleNames wcomp

  --todo hack; make this into a proper withXXX function
  res <- with (WestonHeadlessBackendConfig (WestonBackendConfig westonHeadlessBackendConfigVersion (sizeOf (undefined :: WestonHeadlessBackendConfig)))
           False) $ weston_compositor_load_backend wcomp WestonBackendHeadless . castPtr

  when (res > 0) $ ioError $ userError "Error when loading backend"
  
  socketName <- wl_display_add_socket_auto wldp
  putStrLn $ "Socket: " ++ socketName
  setEnv "WAYLAND_DISPLAY" socketName

  mainLayer <- newWestonLayer wcomp
  weston_layer_set_position mainLayer WestonLayerPositionNormal

  atomically $ writeTVar (_gwcNormalLayer compositor) mainLayer

  
  windowedApi <- weston_windowed_output_get_api wcomp

  let outputPendingSignal = westonCompositorOutputPendingSignal wcomp
  outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi compositor)
  addListenerToSignal outputPendingSignal outputPendingPtr

  let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
  outputCreatedPtr <- createNotifyFuncPtr (onOutputCreated compositor)
  addListenerToSignal outputCreatedSignal outputCreatedPtr

  --createFlushDamageFunc (onFlushDamage compositor) >>= setFlushDamageFunc wcomp 

  westonWindowedOutputCreate windowedApi wcomp "Godot"

  output <- atomically $ readTVar (_gwcOutput compositor)

  forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000

  let api = defaultWestonDesktopApi {
        apiSurfaceAdded = onSurfaceCreated compositor,
        apiSurfaceRemoved = onSurfaceDestroyed compositor,
        apiCommitted = onSurfaceCommit compositor
        }

  
  westonDesktopCreate wcomp api nullPtr

  seat <- newSeat wcomp "Godot"
  weston_seat_init_pointer seat
  weston_seat_init_keyboard seat (XkbKeymap nullPtr)

  installHandler sigUSR1 Ignore Nothing
  {-wet_load_xwayland wcomp-}

  weston_compositor_wake wcomp
  putStrLn "starting compositor"
  wl_display_run wldp

  where
    onOutputPending windowedApi compositor _ outputPtr = do
      putStrLn "output pending"
      let output = WestonOutput $ castPtr outputPtr
      weston_output_set_scale output 1
      weston_output_set_transform output 0
      westonWindowedOutputSetSize windowedApi output 1280 720
      weston_output_enable output
      return ()


    onOutputCreated compositor _ outputPtr = do
      putStrLn "output created"
      let output = WestonOutput $ castPtr outputPtr
      atomically $ writeTVar (_gwcOutput compositor) output
    

    onSurfaceCreated compositor desktopSurface  _ = do
      putStrLn "onSurfaceCreated"
      surface <- weston_desktop_surface_get_surface desktopSurface
      view <- weston_desktop_surface_create_view desktopSurface
      output <- atomically $ readTVar (_gwcOutput compositor)
      westonViewSetOutput view output
      layer <- atomically $ readTVar (_gwcNormalLayer compositor)
      weston_layer_entry_insert  (westonLayerViewList layer) (westonViewLayerEntry view)

      gwst <- newGodotWestonSurfaceTexture

      setWestonSurface gwst surface view

      seat <- getSeat compositor
      sprite <- newGodotWestonSurfaceSprite gwst seat

      G.add_child compositor (safeCast sprite) True

      atomically $ modifyTVar' (_gwcSurfaces compositor) (M.insert surface sprite)

      putStrLn "onSurfaceCreated end"
      return ()


    onSurfaceDestroyed compositor desktopSurface _ = do
      putStrLn "onSurfaceDestroyed"
      surface <- weston_desktop_surface_get_surface desktopSurface
      maybeSprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      case maybeSprite of
        Just sprite -> do 
          Api.godot_object_destroy (safeCast sprite)
          atomically $ modifyTVar' (_gwcSurfaces compositor) (M.delete surface)
        _ -> return ()
      putStrLn "onSurfaceDestroyed end"
      return ()
  
    onSurfaceCommit compositor desktopSurface x y _ = do
      surface <- weston_desktop_surface_get_surface desktopSurface
      Just sprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      updateWestonSurfaceSprite sprite
      move <- spriteShouldMove sprite
      when move $ do
        setSpriteShouldMove sprite False
        moveToUnoccupied compositor sprite

-- TODO: check the origin plane?
moveToUnoccupied :: GodotWestonCompositor -> GodotWestonSurfaceSprite -> IO ()
moveToUnoccupied gwc gwss = do
  surfaces <- atomically $ readTVar (_gwcSurfaces gwc)
  let elems = filter (\x -> (safeCast x :: GodotObject) /= safeCast gwss) $ M.elems surfaces

  extents <- forM elems $ \westonSprite -> do
    sprite <- getSprite westonSprite
    aabb <- G.get_transformed_aabb sprite
    size <- Api.godot_aabb_get_size aabb >>= fromLowLevel
    pos <- Api.godot_aabb_get_position aabb >>= fromLowLevel

    return (pos, size + pos)
  
  let minX = minimum $ 0 : map (view $ _1._x) extents
  let maxX = maximum $ 0 :  map (view $ _2._x) extents
  sprite <- getSprite gwss
  aabb <- G.get_aabb sprite
  size <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX = size ^. _x
  let newPos =
        if abs minX < abs maxX
        then V3 (minX - sizeX/2) 0 0
        else V3 (maxX + sizeX/2) 0 0
  print extents
  print newPos
  tlVec <- toLowLevel newPos
  G.translate gwss tlVec

instance HasBaseClass GodotWestonCompositor where
  type BaseClass GodotWestonCompositor = GodotSpatial
  super (GodotWestonCompositor obj  _ _ _ _ _ _) = GodotSpatial obj

getSeat :: GodotWestonCompositor -> IO WestonSeat
getSeat gwc = do 
  (seat:_) <- atomically (readTVar (_gwcCompositor gwc)) >>= westonCompositorSeats
  return seat

getKeyboard :: GodotWestonCompositor -> IO WestonKeyboard
getKeyboard gwc = getSeat gwc >>= weston_seat_get_keyboard

getPointer :: GodotWestonCompositor -> IO WestonPointer
getPointer gwc = getSeat gwc >>= weston_seat_get_pointer

input :: GodotFunc GodotWestonCompositor
input _ self args = do
  case args !? 0 of
    Just hd -> fromLowLevel hd  >>= \mObj ->
      case fromVariant mObj of
        Just obj -> processInputEvent obj
        _ -> putStrLn "expected input event in first arg of _input"
    _ -> putStrLn "expected argument in _input"
  toLowLevel VariantNil
  where
    setInputHandled = do
      st <- G.get_tree self
      G.set_input_as_handled st
    processInputEvent = processKeyEvent

    processKeyEvent ev = whenM (ev `is_class` "InputEventKey") $ do
      let ev' = GodotInputEventKey ev
      kbd <- getKeyboard self
      godotcode <- G.get_scancode ev'
      case M.lookup godotcode keyTranslation of
        Just code -> do
          wldp <- atomically $ readTVar (_gwcWlDisplay self)
          serial <- wl_display_next_serial wldp

          altPressed <- fromEnum <$> G.get_alt ev'
          shiftPressed <- fromEnum <$> G.get_shift ev'
          ctrlPressed <- fromEnum <$> G.get_control ev'
          superPressed <- fromEnum <$> G.get_metakey ev'
          let mods = fromIntegral $ shiftPressed + superPressed * 2 + ctrlPressed * 4 + altPressed * 8

          weston_keyboard_send_modifiers kbd serial mods mods 0 mods
          
          pressed <- G.is_pressed ev'
          time <- getTime Realtime
          let msec = fromIntegral $ toNanoSecs time `div` 1000000
          weston_keyboard_send_key kbd msec (fromIntegral code) (toState pressed)
          
          
          setInputHandled
        Nothing -> return ()

    toState pressed | pressed = WlKeyboardKeyStatePressed
                    | otherwise = WlKeyboardKeyStateReleased

on_button_signal :: GodotFunc GodotWestonCompositor
on_button_signal _ self args = do
  case toList args of
    [buttonVar, controllerVar, pressedVar] -> do
      button <- fromGodotVariant buttonVar
      controllerObj <- fromGodotVariant controllerVar
      Just controller <- tryObjectCast controllerObj
      pressed <- fromGodotVariant pressedVar
      onButton self controller button pressed
    _ -> return ()
  toLowLevel VariantNil

onButton :: GodotWestonCompositor -> GodotSimulaController -> Int -> Bool -> IO ()
onButton self gsc button pressed = do
  if button == OVR_Button_Grip && not pressed -- Release grabbed
  then processGrabEvent self gsc Nothing pressed
  else do
    whenM (G.is_colliding rc) $ do
      G.get_collider rc >>= tryObjectCast @GodotWestonSurfaceSprite >>= \case
        Just sprite -> onSpriteButton sprite
        Nothing -> return ()
 where
  rc = _gscRayCast gsc
  onSpriteButton sprite = G.get_collision_point rc >>= case button of
    OVR_Button_Grip    -> \_ -> processGrabEvent self gsc (Just sprite) pressed
    OVR_Button_Trigger -> processClickEvent sprite (Button pressed BUTTON_LEFT)
    OVR_Button_AppMenu -> processClickEvent sprite (Button pressed BUTTON_RIGHT)
    _ -> \_ -> return ()

process :: GodotFunc GodotWestonCompositor
process _ self _ = do
  atomically (readTVar (_gwcGrabState self))
    >>= handleState
    >>= atomically . writeTVar (_gwcGrabState self)

  toLowLevel VariantNil
 where
  handleState = \case
    Resizing (ct1, ct2) window origDistance -> do
      newpos1 <- G.get_global_transform ct1 >>= Api.godot_transform_get_origin
      newpos2 <- G.get_global_transform ct2 >>= Api.godot_transform_get_origin
      dist <-  realToFrac <$> Api.godot_vector3_distance_to newpos1 newpos2

      let scale = dist/origDistance
      toLowLevel (V3 scale scale scale) >>= G.scale_object_local window

      return $ Resizing (ct1, ct2) window dist

    state -> return state

processGrabEvent :: GodotWestonCompositor -> GodotSimulaController -> Maybe GodotWestonSurfaceSprite -> Bool -> IO ()
processGrabEvent gwcomp gsc maybeWindow pressed = atomically (readTVar (_gwcGrabState gwcomp)) >>= \case
  NoGrab
    | pressed -> case maybeWindow of
        Just window -> setManip (gsc, window)
        Nothing     -> return ()
    | otherwise -> return ()
  Manipulating ct1@(gsc1, curWindow)
    | pressed -> case maybeWindow of
        Just window
          | gsc == gsc1         -> putStrLn "Tried to press with the same controller without releasing"
          | window == curWindow -> startResize gsc1 gsc window -- Second controller, same window
          | otherwise           -> setManipDual ct1 (gsc, window)
        Nothing -> return ()
    | otherwise -> unset
  ManipulatingDual ct1@(gsc1, _) ct2@(gsc2, _)
    | not pressed && gsc == gsc2 -> setManip ct1
    | not pressed && gsc == gsc1 -> setManip ct2
    | otherwise -> putStrLn "Invalid: Input from a third controller"
  Resizing (gsc1, gsc2) w _
    | not pressed && gsc == gsc2 -> setManip (gsc1, w)
    | not pressed && gsc == gsc1 -> setManip (gsc2, w)
    | otherwise -> putStrLn "Invalid: Input from a third controller"
 where
  setManip ct =
    atomically $ writeTVar (_gwcGrabState gwcomp) $ Manipulating ct

  setManipDual ct1 ct2 =
    atomically $ writeTVar (_gwcGrabState gwcomp) $ ManipulatingDual ct1 ct2

  unset =
    atomically $ writeTVar (_gwcGrabState gwcomp) NoGrab

  startResize gsc1 gsc2 curWindow = do
    -- Position of this controller
    pos1 <- G.get_global_transform gsc1 >>= Api.godot_transform_get_origin
    -- Position of other controller
    pos2 <- G.get_global_transform gsc2 >>= Api.godot_transform_get_origin

    dist <- realToFrac <$> Api.godot_vector3_distance_to pos1 pos2
    atomically $ writeTVar (_gwcGrabState gwcomp) $ Resizing (gsc1, gsc2) curWindow dist


pattern OVR_Button_Touchpad :: Int
pattern OVR_Button_Touchpad = 14

pattern OVR_Button_Trigger :: Int
pattern OVR_Button_Trigger = 15

pattern OVR_Button_AppMenu :: Int
pattern OVR_Button_AppMenu = 1

pattern OVR_Button_Grip :: Int
pattern OVR_Button_Grip = 2

keyTranslation :: M.Map Int Int
keyTranslation = M.fromList 
  [
    ((1 `shiftL` 24) .|. 0x01, 1 ),        -- KEY_ESCAPE         ~ KEY_ESC           
    ((1 `shiftL` 24) .|. 0x02, 15 ),       -- KEY_TAB            ~ KEY_TAB           
    ((1 `shiftL` 24) .|. 0x04, 14 ),       -- KEY_BACKSPACE      ~ KEY_BACKSPACE     
    ((1 `shiftL` 24) .|. 0x05, 28 ),       -- KEY_ENTER          ~ KEY_ENTER         
    ((1 `shiftL` 24) .|. 0x06, 96 ),       -- KEY_KP_ENTER       ~ KEY_KPENTER       
    ((1 `shiftL` 24) .|. 0x07, 110 ),      -- KEY_INSERT         ~ KEY_INSERT        
    ((1 `shiftL` 24) .|. 0x08, 111 ),      -- KEY_DELETE         ~ KEY_DELETE        
    ((1 `shiftL` 24) .|. 0x09, 119 ),      -- KEY_PAUSE          ~ KEY_PAUSE         
    ((1 `shiftL` 24) .|. 0x0A, 210 ),      -- KEY_PRINT          ~ KEY_PRINT         
    ((1 `shiftL` 24) .|. 0x0B, 99 ),       -- KEY_SYSREQ         ~ KEY_SYSRQ         
    ((1 `shiftL` 24) .|. 0x0C, 0x163 ),    -- KEY_CLEAR          ~ KEY_CLEAR         
    ((1 `shiftL` 24) .|. 0x0D, 102 ),      -- KEY_HOME           ~ KEY_HOME          
    ((1 `shiftL` 24) .|. 0x0E, 107 ),      -- KEY_END            ~ KEY_END           
    ((1 `shiftL` 24) .|. 0x0F, 105 ),      -- KEY_LEFT           ~ KEY_LEFT          
    ((1 `shiftL` 24) .|. 0x10, 103 ),      -- KEY_UP             ~ KEY_UP            
    ((1 `shiftL` 24) .|. 0x11, 106 ),      -- KEY_RIGHT          ~ KEY_RIGHT         
    ((1 `shiftL` 24) .|. 0x12, 108 ),      -- KEY_DOWN           ~ KEY_DOWN          
    ((1 `shiftL` 24) .|. 0x13, 104 ),      -- KEY_PAGEUP         ~ KEY_PAGEUP        
    ((1 `shiftL` 24) .|. 0x14, 109 ),      -- KEY_PAGEDOWN       ~ KEY_PAGEDOWN      
    ((1 `shiftL` 24) .|. 0x15, 42 ),       -- KEY_SHIFT          ~ KEY_LEFTSHIFT     
    ((1 `shiftL` 24) .|. 0x16, 29 ),       -- KEY_CONTROL        ~ KEY_LEFTCTRL      
    ((1 `shiftL` 24) .|. 0x17, 125 ),      -- KEY_META           ~ KEY_LEFTMETA      
    ((1 `shiftL` 24) .|. 0x18, 56 ),       -- KEY_ALT            ~ KEY_LEFTALT       
    ((1 `shiftL` 24) .|. 0x19, 58 ),       -- KEY_CAPSLOCK       ~ KEY_CAPSLOCK      
    ((1 `shiftL` 24) .|. 0x1A, 69 ),       -- KEY_NUMLOCK        ~ KEY_NUMLOCK       
    ((1 `shiftL` 24) .|. 0x1B, 70 ),       -- KEY_SCROLLLOCK     ~ KEY_SCROLLLOCK    
    ((1 `shiftL` 24) .|. 0x1C, 59 ),       -- KEY_F1             ~ KEY_F1            
    ((1 `shiftL` 24) .|. 0x1D, 60 ),       -- KEY_F2             ~ KEY_F2            
    ((1 `shiftL` 24) .|. 0x1E, 61 ),       -- KEY_F3             ~ KEY_F3            
    ((1 `shiftL` 24) .|. 0x1F, 62 ),       -- KEY_F4             ~ KEY_F4            
    ((1 `shiftL` 24) .|. 0x20, 63 ),       -- KEY_F5             ~ KEY_F5            
    ((1 `shiftL` 24) .|. 0x21, 64 ),       -- KEY_F6             ~ KEY_F6            
    ((1 `shiftL` 24) .|. 0x22, 65 ),       -- KEY_F7             ~ KEY_F7            
    ((1 `shiftL` 24) .|. 0x23, 66 ),       -- KEY_F8             ~ KEY_F8            
    ((1 `shiftL` 24) .|. 0x24, 67 ),       -- KEY_F9             ~ KEY_F9            
    ((1 `shiftL` 24) .|. 0x25, 68 ),       -- KEY_F10            ~ KEY_F10           
    ((1 `shiftL` 24) .|. 0x26, 87 ),       -- KEY_F11            ~ KEY_F11           
    ((1 `shiftL` 24) .|. 0x27, 88 ),       -- KEY_F12            ~ KEY_F12           
    ((1 `shiftL` 24) .|. 0x28, 183 ),      -- KEY_F13            ~ KEY_F13           
    ((1 `shiftL` 24) .|. 0x29, 184 ),      -- KEY_F14            ~ KEY_F14           
    ((1 `shiftL` 24) .|. 0x2A, 185 ),      -- KEY_F15            ~ KEY_F15           
    ((1 `shiftL` 24) .|. 0x2B, 186 ),      -- KEY_F16            ~ KEY_F16           
    ((1 `shiftL` 24) .|. 0x81, 55 ),       -- KEY_KP_MULTIPLY    ~ KEY_KPASTERISK    
    ((1 `shiftL` 24) .|. 0x82, 98 ),       -- KEY_KP_DIVIDE      ~ KEY_KPSLASH       
    ((1 `shiftL` 24) .|. 0x83, 74 ),       -- KEY_KP_SUBTRACT    ~ KEY_KPMINUS       
    ((1 `shiftL` 24) .|. 0x84, 83 ),       -- KEY_KP_PERIOD      ~ KEY_KPDOT         
    ((1 `shiftL` 24) .|. 0x85, 78 ),       -- KEY_KP_ADD         ~ KEY_KPPLUS        
    ((1 `shiftL` 24) .|. 0x86, 82 ),       -- KEY_KP_0           ~ KEY_KP0           
    ((1 `shiftL` 24) .|. 0x87, 79 ),       -- KEY_KP_1           ~ KEY_KP1           
    ((1 `shiftL` 24) .|. 0x88, 80 ),       -- KEY_KP_2           ~ KEY_KP2           
    ((1 `shiftL` 24) .|. 0x89, 81 ),       -- KEY_KP_3           ~ KEY_KP3           
    ((1 `shiftL` 24) .|. 0x8A, 75 ),       -- KEY_KP_4           ~ KEY_KP4           
    ((1 `shiftL` 24) .|. 0x8B, 76 ),       -- KEY_KP_5           ~ KEY_KP5           
    ((1 `shiftL` 24) .|. 0x8C, 77 ),       -- KEY_KP_6           ~ KEY_KP6           
    ((1 `shiftL` 24) .|. 0x8D, 71 ),       -- KEY_KP_7           ~ KEY_KP7           
    ((1 `shiftL` 24) .|. 0x8E, 72 ),       -- KEY_KP_8           ~ KEY_KP8           
    ((1 `shiftL` 24) .|. 0x8F, 73 ),       -- KEY_KP_9           ~ KEY_KP9           
    ((1 `shiftL` 24) .|. 0x2E, 139 ),      -- KEY_MENU           ~ KEY_MENU          
    ((1 `shiftL` 24) .|. 0x31, 138 ),      -- KEY_HELP           ~ KEY_HELP          
    ((1 `shiftL` 24) .|. 0x32, 153 ),      -- KEY_DIRECTION_L    ~ KEY_DIRECTION     
    ((1 `shiftL` 24) .|. 0x33, 153 ),      -- KEY_DIRECTION_R    ~ KEY_DIRECTION     
    ((1 `shiftL` 24) .|. 0x40, 158 ),      -- KEY_BACK           ~ KEY_BACK          
    ((1 `shiftL` 24) .|. 0x41, 159 ),      -- KEY_FORWARD        ~ KEY_FORWARD       
    ((1 `shiftL` 24) .|. 0x42, 128 ),      -- KEY_STOP           ~ KEY_STOP          
    ((1 `shiftL` 24) .|. 0x43, 173 ),      -- KEY_REFRESH        ~ KEY_REFRESH       
    ((1 `shiftL` 24) .|. 0x44, 114 ),      -- KEY_VOLUMEDOWN     ~ KEY_VOLUMEDOWN    
    ((1 `shiftL` 24) .|. 0x45, 113 ),      -- KEY_VOLUMEMUTE     ~ KEY_MUTE          
    ((1 `shiftL` 24) .|. 0x46, 115 ),      -- KEY_VOLUMEUP       ~ KEY_VOLUMEUP      
    ((1 `shiftL` 24) .|. 0x47, 209 ),      -- KEY_BASSBOOST      ~ KEY_BASSBOOST     
    ((1 `shiftL` 24) .|. 0x51, 172 ),      -- KEY_HOMEPAGE       ~ KEY_HOMEPAGE      
    ((1 `shiftL` 24) .|. 0x52, 0x16c ),    -- KEY_FAVORITES      ~ KEY_FAVORITES     
    ((1 `shiftL` 24) .|. 0x53, 217 ),      -- KEY_SEARCH         ~ KEY_SEARCH        
    ((1 `shiftL` 24) .|. 0x54, 142 ),      -- KEY_STANDBY        ~ KEY_SLEEP         
    ((1 `shiftL` 24) .|. 0x55, 134 ),      -- KEY_OPENURL        ~ KEY_OPEN          
    ((1 `shiftL` 24) .|. 0x56, 155 ),      -- KEY_LAUNCHMAIL     ~ KEY_MAIL          
    ((1 `shiftL` 24) .|. 0xFFFFFF , 240 ), -- KEY_UNKNOWN        ~ KEY_UNKNOWN       
    (0x0020, 57 ),                   -- KEY_SPACE          ~ KEY_SPACE         
    (0x0023, 0x20b ),                -- KEY_NUMBERSIGN     ~ KEY_NUMERIC_POUND 
    (0x0024, 0x1b2 ),                -- KEY_DOLLAR         ~ KEY_DOLLAR        
    (0x0027, 40 ),                   -- KEY_APOSTROPHE     ~ KEY_APOSTROPHE    
    (0x002C, 51 ),                   -- KEY_COMMA          ~ KEY_COMMA         
    (0x002D, 12 ),                   -- KEY_MINUS          ~ KEY_MINUS         
    (0x002E, 52 ),                   -- KEY_PERIOD         ~ KEY_DOT           
    (0x0030, 11 ),                   -- KEY_0              ~ KEY_0             
    (0x0031, 2 ),                    -- KEY_1              ~ KEY_1             
    (0x0032, 3 ),                    -- KEY_2              ~ KEY_2             
    (0x0033, 4 ),                    -- KEY_3              ~ KEY_3             
    (0x0034, 5 ),                    -- KEY_4              ~ KEY_4             
    (0x0035, 6 ),                    -- KEY_5              ~ KEY_5             
    (0x0036, 7 ),                    -- KEY_6              ~ KEY_6             
    (0x0037, 8 ),                    -- KEY_7              ~ KEY_7             
    (0x0038, 9 ),                    -- KEY_8              ~ KEY_8             
    (0x0039, 10 ),                   -- KEY_9              ~ KEY_9             
    (0x003B, 39 ),                   -- KEY_SEMICOLON      ~ KEY_SEMICOLON     
    (0x003D, 13 ),                   -- KEY_EQUAL          ~ KEY_EQUAL         
    (0x003F, 214 ),                  -- KEY_QUESTION       ~ KEY_QUESTION      
    (0x0041, 30 ),                   -- KEY_A              ~ KEY_A             
    (0x0042, 48 ),                   -- KEY_B              ~ KEY_B             
    (0x0043, 46 ),                   -- KEY_C              ~ KEY_C             
    (0x0044, 32 ),                   -- KEY_D              ~ KEY_D             
    (0x0045, 18 ),                   -- KEY_E              ~ KEY_E             
    (0x0046, 33 ),                   -- KEY_F              ~ KEY_F             
    (0x0047, 34 ),                   -- KEY_G              ~ KEY_G             
    (0x0048, 35 ),                   -- KEY_H              ~ KEY_H             
    (0x0049, 23 ),                   -- KEY_I              ~ KEY_I             
    (0x004A, 36 ),                   -- KEY_J              ~ KEY_J             
    (0x004B, 37 ),                   -- KEY_K              ~ KEY_K             
    (0x004C, 38 ),                   -- KEY_L              ~ KEY_L             
    (0x004D, 50 ),                   -- KEY_M              ~ KEY_M             
    (0x004E, 49 ),                   -- KEY_N              ~ KEY_N             
    (0x004F, 24 ),                   -- KEY_O              ~ KEY_O             
    (0x0050, 25 ),                   -- KEY_P              ~ KEY_P             
    (0x0051, 16 ),                   -- KEY_Q              ~ KEY_Q             
    (0x0052, 19 ),                   -- KEY_R              ~ KEY_R             
    (0x0053, 31 ),                   -- KEY_S              ~ KEY_S             
    (0x0054, 20 ),                   -- KEY_T              ~ KEY_T             
    (0x0055, 22 ),                   -- KEY_U              ~ KEY_U             
    (0x0056, 47 ),                   -- KEY_V              ~ KEY_V             
    (0x0057, 17 ),                   -- KEY_W              ~ KEY_W             
    (0x0058, 45 ),                   -- KEY_X              ~ KEY_X             
    (0x0059, 21 ),                   -- KEY_Y              ~ KEY_Y             
    (0x005A, 44 ),                   -- KEY_Z              ~ KEY_Z             
    (0x005C, 43 ),                   -- KEY_BACKSLASH      ~ KEY_BACKSLASH     
    (0x0060, 41 ),                   -- KEY_QUOTELEFT      ~ KEY_GRAVE         
    (0x007B, 26 ),                   -- KEY_BRACELEFT      ~ KEY_LEFTBRACE     
    (0x007D, 27 ),                   -- KEY_BRACERIGHT     ~ KEY_RIGHTBRACE    
    (0x00A0, 0x19b ),                -- KEY_NOBREAKSPACE   ~ KEY_BREAK         
    (0x00A4, 0x1b3 ),                -- KEY_CURRENCY       ~ KEY_EURO          
    (0x00A5, 124 ),                  -- KEY_YEN            ~ KEY_YEN           
    (0x00B1, 118 )                   -- KEY_PLUSMINUS      ~ KEY_KPPLUSMINUS   
  ]
