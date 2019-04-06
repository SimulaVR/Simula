{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Plugin.SimulaServer where

import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript
import           Godot.Extra.Register

import qualified Data.Map.Strict as M

import Plugin.Input
import Plugin.SimulaViewSprite
import Plugin.SimulaViewTexture

import Control.Monad
import Control.Concurrent
import System.Environment

import System.Process

import Telemetry

import           Debug.Trace
import           Control.Lens hiding (Context)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Data.Maybe
import           Data.List
import           Data.Coerce

import           Foreign hiding (void)
import           Foreign.C.Error
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Text.XkbCommon.Keysym
import           Text.XkbCommon.KeyboardState
import           Text.XkbCommon.InternalTypes
import           Text.XkbCommon.Context
import           Text.XkbCommon.Keymap

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
import           Graphics.Wayland.WlRoots.Compositor
import           Graphics.Wayland.WlRoots.Output
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.WlRoots.Backend
import           Graphics.Wayland.WlRoots.Backend.Headless
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.Render
import           Graphics.Wayland.WlRoots.Render.Color
import           Graphics.Wayland.WlRoots.OutputLayout
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Seat
import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.XCursorManager
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Input.Keyboard
import           Graphics.Wayland.WlRoots.Input.Pointer
import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.Input.Buttons
import           Graphics.Wayland.WlRoots.Box
import           Graphics.Wayland.WlRoots.Util
import           Graphics.Wayland.WlRoots.DeviceManager

import           System.Clock
import           Control.Monad.Extra

initializeSimulaCtxAndIncludes

-- To (probably) be used with Simula's resizing function (to make sure we stop propogating wayland events during grabs).
-- data SimulaCursorMode = SimulaCursorPassthrough | SimulaCursorMove |  SimulaCursorResize

instance GodotClass GodotSimulaServer where
  godotClassName = "SimulaServer"

instance ClassExport GodotSimulaServer where
  classInit obj  = initGodotSimulaServer obj

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" Plugin.SimulaServer.ready
    , GodotMethod NoRPC "_input" Plugin.SimulaServer.input
    ]

  -- Test:
  classSignals = [ signal "test_signal1" [("arg1", GodotVariantTypeVector3), ("arg2", GodotVariantTypeObject)]
                 , signal "test_signal2" []
                 ]

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _) = GodotSpatial obj

ready :: GFunc GodotSimulaServer
ready gss _ = do
  startSimulaServerOnNewThread gss
  startTelemetry (gss ^. gssViews) -- Doesn't seem to require previous function to get to displayRunServer
  toLowLevel VariantNil

input :: GFunc GodotSimulaServer
input self args = do
  (getArg' 0 args :: IO GodotObject)
    >>= asClass GodotInputEventKey "InputEventKey" >>= \case
      Just evk -> do
        processKeyEvent self evk
        setInputHandled self

      Nothing  -> return () -- not a key
  toLowLevel VariantNil

-- TODO: check the origin plane?
moveToUnoccupied :: GodotSimulaServer -> GodotSimulaViewSprite -> IO ()
moveToUnoccupied gss gsvs = do
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

-- Should probably be called "mutateServerForGrabOperation".
beginInteractive :: IO ()
beginInteractive =
  putStrLn
    "beginInteractive should mutate the server state to reflect \
    \that we're in the middle of a move/resize operation; \
    \for now we leave unimplemented."
-- Real implementation should be this:
  -- 1. Gets ~seat->pointer_state.focused_surface~
  -- 2. Gets ~SimulaView~'s wlr_surface, and makes sure equal to (1).
  -- 3. If so, mutates tsGrabbedView, tsCursorMode, tsGrab, tsGrabWidth, tsGrabHeight, tsResizeEdges accordingly

-- | A wl_listener that (i) sets the SimulaKeyboard as active in the seat (since
-- | wayland forces us to have one active keyboard as a time per seat) and (ii) sends
-- | the modifier keys to the client.
keyboardHandleModifiers :: SimulaKeyboard -> WlListener ()
keyboardHandleModifiers simulaKeyboard = WlListener $ \_ ->
  do let ptrWlrSeat     = (_gssSeat (_skServer simulaKeyboard))
     let ptrInputDevice = (_skDevice simulaKeyboard)

     seatSetKeyboard ptrWlrSeat ptrInputDevice -- Converts ptrInputDevice to wlr_keyboard and makes it the active keyboard for the seat
     seatKeyboardNotifyModifiers ptrWlrSeat -- Sends modifiers to the client

     where seatKeyboardNotifyModifiers ptrWlrSeat = do
                maybePtrWlrKeyboard <- getSeatKeyboard ptrWlrSeat
                case maybePtrWlrKeyboard of
                    Nothing             -> putStrLn "Couldn't get keyboard!"
                    Just ptrWlrKeyboard -> do
                      (keycodes, numKeycodes) <- getKeyboardKeys ptrWlrKeyboard
                      let modifiers = getModifierPtr ptrWlrKeyboard
                      keyboardNotifyModifiers ptrWlrSeat modifiers

-- | This function is the handler for key presses. It (i) sets the keyboard as the
-- | active one in the wayland seat and (ii) notifies the client about the keypress.
keyboardHandleKey :: SimulaKeyboard -> WlListener EventKey
keyboardHandleKey simulaKeyboard = WlListener $ \ptrEventKey ->
  do event               <- (peek ptrEventKey) -- EventKey ∈ Storable
     let server          =  (_skServer simulaKeyboard)
     let seat            =  (_gssSeat server)
     let device          =  (_skDevice simulaKeyboard)
     let eventKeyState   =  (state event)
     let eventKeyTimeSec =  (timeSec event)
     let eventKeyKeyCode =  (keyCode event)

     -- Here is where we could check if the key event is a compositor-level shortcut
     -- i.e., an <alt> + <key> combo, and process it via `handleKeybinding`.

     seatSetKeyboard seat device
     keyboardNotifyKey seat eventKeyTimeSec eventKeyKeyCode eventKeyState

-- | This function "initializes" a wlr_input_device (second argument) into a
-- | wlr_keyboard. To do so it (i) constructs a bunch of XKB state to load into the
-- | keyboard; (ii) sets the keyboard's repeat info; (iii) attaches the
-- | keyboardHandleModifiers & keyboardHandleKey wl_listener's to the wlr_keyboard's
-- | "key" and "modifiers" signals respectively. It finally sets the keyboard as
-- | active (relative to our seat) and adds it to the head of the server's keyboard
-- | list.
serverNewKeyboard :: GodotSimulaServer -> Ptr InputDevice -> IO ()
serverNewKeyboard server device = do
  deviceType    <- inputDeviceType device
  maybeXkbContext <- newContext defaultFlags
  case (deviceType, maybeXkbContext) of
       ((DeviceKeyboard ptrWlrKeyboard), (Just xkbContext)) -> mdo [tokenModifiers, tokenKey] <- handleKeyboard simulaKeyboard ptrWlrKeyboard xkbContext
                                                                   let simulaKeyboard = SimulaKeyboard { _skServer    = server         :: GodotSimulaServer
                                                                                                       , _skDevice    = device         :: Ptr InputDevice
                                                                                                       , _skModifiers = tokenModifiers :: ListenerToken
                                                                                                       , _skKey       = tokenKey       :: ListenerToken
                                                                                                       }
                                                                   makeKeyboardActiveHead simulaKeyboard
                                                                   return ()
       _ -> putStrLn "Failed to get keyboard!"
  where handleKeyboard :: SimulaKeyboard -> Ptr WlrKeyboard -> Context -> IO ([ListenerToken])
        handleKeyboard simulaKeyboard deviceKeyboard context = do
          maybeKeymap <- newKeymapFromNames context noPrefs
          case maybeKeymap of
               Nothing       -> putStrLn "Failed to get keymap!" >> return []
               (Just keymap) -> do setKeymapAndRepeatInfo deviceKeyboard keymap
                                   tokens <- handleSignals simulaKeyboard deviceKeyboard
                                   return tokens

        setKeymapAndRepeatInfo :: Ptr WlrKeyboard -> Keymap -> IO ()
        setKeymapAndRepeatInfo keyboard keymap = do
          withKeymap keymap (\keyMapC -> setKeymap keyboard keyMapC)
          let keyboard' = toInlineC keyboard
          [C.block| void { wlr_keyboard_set_repeat_info($(struct wlr_keyboard * keyboard'), 25, 600); }|] -- hsroots doesn't provide this call.

        handleSignals :: SimulaKeyboard -> Ptr WlrKeyboard -> IO ([ListenerToken])
        handleSignals simulaKeyboard keyboard = do
          let keyboardSignals = getKeySignals keyboard
          let keySignalKey' = (keySignalKey keyboardSignals)
          let keySignalModifiers' = (keySignalModifiers keyboardSignals)
          tokenModifiers <- addListener (keyboardHandleModifiers simulaKeyboard) keySignalModifiers' -- a = SimulaKeyboard
          tokenKey <- addListener (keyboardHandleKey simulaKeyboard)       keySignalKey' -- a = EventKey

          let tokens = [tokenModifiers, tokenKey]
          return tokens

        makeKeyboardActiveHead :: SimulaKeyboard -> IO ()
        makeKeyboardActiveHead simulaKeyboard = do
          seatSetKeyboard (_gssSeat server) (_skDevice simulaKeyboard)
          keyboardList <- atomically $ readTVar (_gssKeyboards server)
          atomically $ writeTVar (_gssKeyboards server) ([simulaKeyboard] ++ keyboardList)

-- | This wl_listener is called when Simula gets a new wlr_input_device. We (i)
-- | inspect the wlr_input_device to see whether it is a keyboard or a pointer,
-- | passing it to serverNew* accordingly; (ii) set the wlr_seat's "capabilities" as
-- | either "mouse" or "mouse + keyboard", depending upon whether the server's
-- | keyboard list is empty or not.
serverNewInput :: GodotSimulaServer -> WlListener InputDevice
serverNewInput simulaServer = WlListener $ \device ->
  do deviceType <- inputDeviceType device
     let seat = (_gssSeat simulaServer)
     return ()
     case deviceType of
         (DeviceKeyboard _) -> (serverNewKeyboard simulaServer device)
         _                  -> putStrLn "Simula does not know how to handle this input type."
         -- (DevicePointer  _) -> (serverNewPointer simulaServer  device)

     keyboardList <- atomically $ readTVar (_gssKeyboards simulaServer)

     -- Note that Graphics.Wayland.Internal.SpliceServerTypes code generates the following:
     --   newtype SeatCapability = SeatCapability GHC.Types.Int
     -- Now see: https://github.com/swaywm/hsroots/blob/f9b07af96dff9058a3aac59eba5a608a91801c0a/src/Graphics/Wayland/WlRoots/Input.hsc#L48
     -- EDIT: We cannot use deviceToInt as it gets us WLR_* enums and we need WL_SEAT_* enums.
     keyboardCapability' <- fromIntegral <$> [C.exp| int wl_seat_capability { WL_SEAT_CAPABILITY_KEYBOARD } |]
     let keyboardCapabilities = [SeatCapability keyboardCapability']
     setSeatCapabilities seat keyboardCapabilities

-- | This is an event handler raised by the backend when a new output (i.e.,
-- | display or monitor) becomes available. Simula doesn't require an output
-- | (since Godot handles our rendering for us), but I include this in case
-- | clients need to see a some sort of output global before rendering.
serverNewOutput :: GodotSimulaServer -> WlListener WlrOutput
serverNewOutput simulaServer = WlListener $ \ptrWlrOutput -> mdo
    -- Sets the outputs (width, height, refresh rate) which depends on your hardware.
    -- This is only necessary for some backends (i.e., DRM+KMS). Here we just pick
    -- the first mode supported in the list retrieved. Unclear if we need this or not
    -- with a headless backend.
    setOutputModeAutomatically ptrWlrOutput

    let simulaOutput = SimulaOutput { _soServer = simulaServer
                                    , _soWlrOutput = ptrWlrOutput
                                    }
    -- Add output to head of server's output list
    outputsList  <- atomically $ readTVar (simulaServer ^. gssOutputs)
    let outputsListNew = simulaOutput:outputsList
    atomically $ writeTVar (simulaServer ^. gssOutputs) outputsListNew

    -- Adds the wl_output global to the display, which Wayland clients can use to
    -- find out information about this output (such as DPI, scale factor,
    -- manufacturerer, etc). We use it for dummy purposes in Simula.
    createOutputGlobal ptrWlrOutput

    where setOutputModeAutomatically :: Ptr WlrOutput -> IO ()
          setOutputModeAutomatically ptrWlrOutput = do
            hasModes' <- hasModes ptrWlrOutput
            when hasModes' $ do modes <- getModes ptrWlrOutput
                                let headMode = head modes -- We might want to reverse this list first to mimic C implementation
                                setOutputMode headMode ptrWlrOutput
                                return ()

-- | Called when the surface is "mapped" (i.e., "ready to display
-- | on-screen"). Note that the actual "mapping" and "unmapping"
-- | occurs in serverNewXdgSurface and xdgSurfaceDestroy respectively.
xdgSurfaceMap :: SimulaView -> WlListener WlrXdgSurface
xdgSurfaceMap simulaView = WlListener $ \_ -> do
  maybeSurface <- xdgSurfaceGetSurface (simulaView ^. svXdgSurface)

  -- Set the XDG surface as mapped
  atomically $ writeTVar (simulaView ^. svMapped) True

  -- Then give it keyboard focus
  case maybeSurface of
    Nothing -> return ()
    Just surface -> focusView simulaView surface
  return ()

-- | Called when the surface is "unmapped", and should no longer be shown.
xdgSurfaceUnmap :: SimulaView -> WlListener WlrXdgSurface
xdgSurfaceUnmap simulaView = WlListener $ \_ -> do
  atomically $ writeTVar (simulaView ^. svMapped) False

-- | Called when the surface is destroyed, and should never be shown again.
xdgSurfaceDestroy :: SimulaView -> WlListener WlrXdgSurface
xdgSurfaceDestroy simulaView = WlListener $ \_ -> do
  -- Free the view's WlListener tokens.
  freeListenerToken (simulaView ^. svMap)
  freeListenerToken (simulaView ^. svUnmap)
  freeListenerToken (simulaView ^. svDestroy)

  -- Clean up the associated GodotSimulaViewSprite and
  -- remove the `view ↦ sprite` mapping from the server.
  let gss = simulaView ^. svServer
  maybeSprite <- M.lookup simulaView <$> atomically (readTVar (_gssViews gss))
  case maybeSprite of
    Just sprite -> do
      Api.godot_object_destroy (safeCast sprite)
      atomically $ modifyTVar' (_gssViews gss) (M.delete simulaView)
    _ -> return ()

-- This is a key function in Simula. Every time a client commits a frame to the
-- server for rendering, this handler is called, which ultimately ensures the view's
-- latest surface buffer is pasted onto the Godot sprite.
serverSurfaceCommit :: SimulaView -> WlListener WlrSurface
serverSurfaceCommit simulaView = WlListener $ \ptrWlrSurface -> do
  let gss = simulaView ^. svServer
  Just gsvs <- M.lookup simulaView <$> atomically (readTVar (_gssViews gss))

  updateSimulaViewSprite gsvs -- Wraps updateSimulaViewTexture, which sends wlr buffer to Godot

  whenM (spriteShouldMove gsvs) $ do
    atomically $ writeTVar (_gsvsShouldMove gsvs) False
    moveToUnoccupied gss gsvs

  -- Wrap a call to wlr_surface_send_frame_done so that client will start rendering the next frame
  surfaceSendFrameDone ptrWlrSurface
  where surfaceSendFrameDone ptrWlrSurface = do
          let ptrWlrSurface' = toInlineC ptrWlrSurface
          now <- getTime Realtime
          ptrTimeSpecNow <- malloc :: IO (Ptr TimeSpec)
          poke ptrTimeSpecNow now
          [C.exp| void { wlr_surface_send_frame_done( $(struct wlr_surface * ptrWlrSurface'), $(struct timespec * ptrTimeSpecNow)) } |] -- hsroots doesn't provide this function
          free ptrTimeSpecNow

-- | This event is raised when wlr_xdg_shell receives a new xdg surface from a
-- | client, either a toplevel (application window) or popup. For now we ignore popups,
-- | which might cause problem's for Simula's ability to handle popups.
serverNewXdgSurface :: GodotSimulaServer -> WlListener WlrXdgSurface
serverNewXdgSurface gss = WlListener $ \ptrWlrXdgSurface -> do
  putStrLn "serverNewXdgSurface"
  maybeTopLevel   <- getXdgToplevel ptrWlrXdgSurface
  maybeWlrSurface <- xdgSurfaceGetSurface ptrWlrXdgSurface
  case (maybeTopLevel, maybeWlrSurface) of
       (Just topLevel, Just ptrWlrSurface) -> createServerView ptrWlrSurface ptrWlrXdgSurface topLevel
       (_, _) -> return () -- If the surface is, i.e., a popup or has no XDG "role", then we don't do anything.
  where
        -- createServerView creates a new SimulaView, connects its WlListeners
        -- to the proper signals, and adds it to the front of the server's view
        -- list
        createServerView :: Ptr WlrSurface -> Ptr WlrXdgSurface -> Ptr WlrXdgToplevel -> IO ()
        createServerView ptrWlrSurface ptrWlrXdgSurface topLevel = mdo
          -- Signals are divided into XdgSurfaceEvents, XdgTopLevelEvents, & now WlrSurfaceEvents
          let wlrXdgSurfaceEvents = getXdgSurfaceEvents ptrWlrXdgSurface
          let eventToplevelWlrXdgToplevelEvents = getXdgToplevelEvents topLevel
          let wlrSurfaceEvents = getWlrSurfaceEvents ptrWlrSurface

          -- We omit "timeout", "popup" signals
          let signalDestroy = xdgSurfaceEvtDestroy wlrXdgSurfaceEvents :: Ptr (WlSignal WlrXdgSurface)
          let signalMap = xdgSurfaceEvtMap wlrXdgSurfaceEvents :: Ptr (WlSignal WlrXdgSurface)
          let signalUnmap = xdgSurfaceEvtUnmap wlrXdgSurfaceEvents :: Ptr (WlSignal WlrXdgSurface)

          -- We omit "maximize", "fullscreen", "minimize", and "menu" signals
          let signalToplevelMove                = xdgToplevelEvtMove  eventToplevelWlrXdgToplevelEvents :: Ptr (WlSignal MoveEvent)
          let signalToplevelResize              = xdgToplevelEvtResize  eventToplevelWlrXdgToplevelEvents :: Ptr (WlSignal ResizeEvent)
          

          let signalSurfaceCommit = wlrSurfaceEvtCommit wlrSurfaceEvents :: Ptr (WlSignal WlrSurface)

          svMapToken     <- addListener (xdgSurfaceMap simulaView)     signalMap
          svUnmapToken   <- addListener (xdgSurfaceUnmap simulaView)   signalUnmap
          svDestroyToken <- addListener (xdgSurfaceDestroy simulaView) signalDestroy
          svSurfaceCommitToken <- addListener (serverSurfaceCommit simulaView) signalSurfaceCommit

          falseBoolTVar <- atomically $ (newTVar False) :: IO (TVar Bool)

          let simulaView = SimulaView { _svServer     = gss :: GodotSimulaServer
                                      , _svXdgSurface = ptrWlrXdgSurface     :: Ptr WlrXdgSurface
                                      , _svMapped     = falseBoolTVar        :: TVar Bool
                                      , _svMap        = svMapToken           :: ListenerToken
                                      , _svUnmap      = svUnmapToken         :: ListenerToken
                                      , _svDestroy    = svDestroyToken       :: ListenerToken
                                      , _svCommit     = svSurfaceCommitToken :: ListenerToken
                                      }

          -- We now instantiate Godot types and add the `simulaView ↦ gsvs`
          -- mapping to the server's (M.Map SimulaView GodotSimulaViewSprite)
          gsvt <- newGodotSimulaViewTexture simulaView
          gsvs <- newGodotSimulaViewSprite gsvt -- Potentially a problem: this has the side effect of calling updateSimulaViewTexture twice, and potentially on a null buffer
          G.add_child gsvs (safeCast gsvs) True
          atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs)


initGodotSimulaServer :: GodotObject -> IO (GodotSimulaServer)
initGodotSimulaServer obj = mdo
  displayServer <- throwIfNullPtr displayCreate
  let displayServer' = toInlineC displayServer
  -- The following calls cause EGL errors to bubble up:
    -- backendHeadless <- createHeadlessBackend displayServer
    -- backendAuto <- backendAutocreate displayServer

  -- Thus we replace with wlr_godot_backend_create (see ./cbits/wlr_godot_backend.c)
  backendGodot' <- [C.exp| struct wlr_backend * { wlr_godot_backend_create($(struct wl_display * displayServer'), NULL) }|] :: IO (Ptr C'WlrBackend)
  let backendGodot = toC2HS backendGodot' :: Ptr Backend

  ptrRenderer   <- backendGetRenderer backendGodot
  initWlDisplay displayServer ptrRenderer
  ptrWlrCompositor <- compositorCreate displayServer ptrRenderer -- Potentially unneeded, but perhaps clients need to see this to render?
  ptrWlrDeviceManager <- managerCreate displayServer

  emptyMapTVar <- atomically (newTVar mempty) :: IO (TVar (M.Map SimulaView GodotSimulaViewSprite))
  emptyKeyboardsTVar   <- atomically $ (newTVar []) :: IO (TVar [SimulaKeyboard])
  emptyOutputsTVar     <- atomically $ (newTVar []) :: IO (TVar [SimulaOutput])

  -- Both the following emitted in backend/headless/backend.c:backend_start(),
  -- and hence won't be emitted in our empty gdlwroots.backend_start().
  --   wlr_signal_emit_safe(&wlr_backend.events.new_output, <data payload>);
  --   wlr_signal_emit_safe(&wlr_backend.events.new_input, <data payload>);
  -- which means the following functions are worthless:
    -- let signalBackendNewOutput = backendEvtOutput (backendGetSignals backendGodot)
    -- tokenNewOutput <- addListener (serverNewOutput gss) signalBackendNewOutput
    -- let signalBackendNewInput  = backendEvtInput (backendGetSignals backendGodot)
    -- tokenNewInput <- addListener (serverNewInput gss) signalBackendNewInput
  -- Thus, if we need to add a wlr_output or a wlr_input_device, we will have to do
  -- (i) manually by (ii) implementing wlr_godot_add_[input_device,output] ourselves,
  -- which should involve calls to wlr_signal_emit_safe(..) as above.

  (xdgShell, tokenNewXdgSurface) <- getXdgShellAndToken displayServer gss


  seat <- createSeat displayServer "seat0"

  let gss = GodotSimulaServer {
      _gssObj           = obj                :: GodotObject
    , _gssDisplay       = displayServer      :: DisplayServer
    , _gssViews         = emptyMapTVar       :: TVar (M.Map SimulaView GodotSimulaViewSprite)
    , _gssBackend       = backendGodot       :: Ptr Backend
    , _gssXdgShell      = xdgShell           :: Ptr WlrXdgShell
    , _gssSeat          = seat               :: Ptr WlrSeat
    , _gssKeyboards     = emptyKeyboardsTVar :: TVar [SimulaKeyboard]
    , _gssOutputs       = emptyOutputsTVar   :: TVar [SimulaOutput]
    , _gssRenderer      = ptrRenderer        :: Ptr Renderer
    , _gssNewXdgSurface = tokenNewXdgSurface :: ListenerToken
    -- , _gssNewInput      = tokenNewInput      :: ListenerToken -- Not needed now that we're using wlr_godot_backend
    -- , _gssNewOutput     = tokenNewOutput     :: ListenerToken -- "
  }
  return gss
  where throwIfNullPtr f = throwErrnoIf
                           (\res -> ((coerce res :: Ptr ()) == nullPtr)) -- Throw error if the pointer returned by f is 0 (i.e., NULL)
                           "wl_* initialization failed."
                           f
        getXdgShellAndToken displayServer gss = do
          let displayServer' = toInlineC displayServer
          xdgShell' <- [C.exp| struct wlr_xdg_shell * { wlr_xdg_shell_create($(struct wl_display * displayServer'))} |]
          let xdgShell = toC2HS xdgShell' :: Ptr WlrXdgShell
          xdgShellSignal' <- [C.block| struct wl_signal * {struct wl_signal * signal_ptr;
                                                        signal_ptr = &$(struct wlr_xdg_shell * xdgShell')->events.new_surface;
                                                        return signal_ptr;} |] -- I'm assuming this pointer is freed when we eventually call freeListenerToken on tokenNewXdgSurface
          let xdgShellSignal = (toC2HS xdgShellSignal') :: Ptr (WlSignal WlrXdgSurface)
          tokenNewXdgSurface <- addListener (serverNewXdgSurface gss) xdgShellSignal
          return (xdgShell, tokenNewXdgSurface)

-- | Here we set socket(s), start the backend, and finally call displayRun (which
-- | blocks until the compositor is shut down). Once the compositor is shut down,
-- | we free everything.
startSimulaServerOnNewThread :: GodotSimulaServer -> IO ()
startSimulaServerOnNewThread gss = void $ forkOS $ mdo
      -- prevDisplay <- getEnv "DISPLAY"
      setLogPrio Debug

      let socketName = "simula-0"
      putStrLn $ "Socket: " ++ socketName
      setEnv "WAYLAND_DISPLAY" socketName
      displayAddSocket (gss ^. gssDisplay) (Just socketName)

      -- This just calls backend_start, which with wlr_godot_backend is just a shim
      -- (compare to the backend_start of the headless backend).
      backendStart (gss ^. gssBackend)

      -- In case it matters for debugging: I'm assuming that WlRoots doesn't force
      -- us to change compositor frame internals like we had to with Weston, i.e.:
        -- $(struct weston_compositor * compositor')->repaint_msec = 1000
        -- forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000

      -- setEnv "DISPLAY" prevDisplay
      displayRun (gss ^. gssDisplay)
      -- destroyDisplayClients displayServer
      displayDestroy (gss ^. gssDisplay)
      freeListenerToken (gss ^. gssNewXdgSurface)
      -- freeListenerToken (gss ^. gssNewInput)  -- Not needed now that we're using wlr_godot_backend
      -- freeListenerToken (gss ^. gssNewOutput) -- "

      where destroyDisplayClients displayServer = do
              let displayServer' = toInlineC displayServer
              [C.exp| void { wl_display_destroy_clients($(struct wl_display * displayServer'))} |]

-- This function is in the latest godot-extra, but am using this for now to avoid fixing breaking changes.
fromNativeScript :: GodotObject -> IO a
fromNativeScript = Api.godot_nativescript_get_userdata
  >=> Foreign.deRefStablePtr . Foreign.castPtrToStablePtr

-- | This is a terrible hack. It requires a dummy GodotNode as its first
-- | argument (can possibly cast nullPtr as GodotNode internally?). Moreover, 
-- | it uses a hardcoded path that may change in the future. Finally, it somehow
-- | doesn't return a Maybe type (attempts to access a null Godot instance
-- | will just crash our program). See https://docs.godotengine.org/en/3.0/classes/class_node.html#class-node-get-node
getSimulaServerFromHardcodedNodePath :: GodotNode -> IO (GodotSimulaServer)
getSimulaServerFromHardcodedNodePath node = do
  gss <- getGodotSimualServerFromNodePath (node :: GodotNode) "/root/Root/Simula" -- Confusingly, this path is set in Simula.hs and not this module
  return gss
  where getGodotSimualServerFromNodePath :: GodotNode -> String -> IO GodotSimulaServer
        getGodotSimualServerFromNodePath node nodePathStr = do
          nodePath <- (toLowLevel (pack nodePathStr))
          gssNode <- G.get_node node nodePath
          -- G.print_tree ((safeCast gssNode) :: GodotNode)
          -- let gss = (unsafeCoerce gssNode) :: GodotSimulaServer
          gss <- (Plugin.SimulaServer.fromNativeScript (safeCast gssNode)) :: IO GodotSimulaServer
          return gss
