{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Debug.Marshal where

import System.IO.Unsafe
import Foreign
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map
import Data.Monoid
import Foreign.C.String
import Foreign.C.Types
import Text.RawString.QQ (r)

import Data.Coerce
import Debug.C

-- |HsRoots Modules for reference:
import      Graphics.Wayland.Server
import      Graphics.Wayland.Internal.Server
import      Graphics.Egl
import      Graphics.Pixman
import      Graphics.Wayland.Global
import      Graphics.Wayland.List
import      Graphics.Wayland.Resource
import      Graphics.Wayland.Server.Client
import      Graphics.Wayland.Signal
import      Graphics.Wayland.WlRoots.Backend
import      Graphics.Wayland.WlRoots.Backend.Headless
import      Graphics.Wayland.WlRoots.Backend.Libinput
import      Graphics.Wayland.WlRoots.Backend.Multi
import      Graphics.Wayland.WlRoots.Backend.Session
import      Graphics.Wayland.WlRoots.Box
import      Graphics.Wayland.WlRoots.Buffer
import      Graphics.Wayland.WlRoots.Compositor
import      Graphics.Wayland.WlRoots.Cursor
import      Graphics.Wayland.WlRoots.DeviceManager
import      Graphics.Wayland.WlRoots.ExportDMABuf
import      Graphics.Wayland.WlRoots.Egl
import      Graphics.Wayland.WlRoots.GammaControl
import      Graphics.Wayland.WlRoots.Global
import      Graphics.Wayland.WlRoots.IdleInhibit
import      Graphics.Wayland.WlRoots.Input
import      Graphics.Wayland.WlRoots.Input.Buttons
import      Graphics.Wayland.WlRoots.Input.Keyboard
import      Graphics.Wayland.WlRoots.Input.Pointer
import      Graphics.Wayland.WlRoots.Input.Tablet
import      Graphics.Wayland.WlRoots.Input.TabletPad
import      Graphics.Wayland.WlRoots.Input.TabletTool
import      Graphics.Wayland.WlRoots.Input.Touch
import      Graphics.Wayland.WlRoots.InputInhibitor
import      Graphics.Wayland.WlRoots.LinuxDMABuf
import      Graphics.Wayland.WlRoots.Output
import      Graphics.Wayland.WlRoots.OutputLayout
import      Graphics.Wayland.WlRoots.PrimarySelection
import      Graphics.Wayland.WlRoots.Render
import      Graphics.Wayland.WlRoots.Render.Color
import      Graphics.Wayland.WlRoots.Render.Gles2
import      Graphics.Wayland.WlRoots.Render.Matrix
import      Graphics.Wayland.WlRoots.Screenshooter
import      Graphics.Wayland.WlRoots.Seat
import      Graphics.Wayland.WlRoots.ServerDecoration
import      Graphics.Wayland.WlRoots.Surface
import      Graphics.Wayland.WlRoots.SurfaceLayers
import      Graphics.Wayland.WlRoots.Tabletv2
import      Graphics.Wayland.WlRoots.Util
import      Graphics.Wayland.WlRoots.Util.Region
import      Graphics.Wayland.WlRoots.WlShell
import      Graphics.Wayland.WlRoots.XCursor
import      Graphics.Wayland.WlRoots.XCursorManager
import      Graphics.Wayland.WlRoots.XWayland
import      Graphics.Wayland.WlRoots.XdgShell
-- import      Graphics.Wayland.WlRoots.XdgShellv6

initializeSimulaCtxAndIncludes

-- | We make C'WlListener an instance of Storable so we can conveniently
-- | pass it from C to Haskell and back via inline-C.
-- | NOTE: This actually didn't work; see:
-- | https://stackoverflow.com/questions/54375088/marshalling-a-struct-from-c-to-haskell-using-inline-c
instance Storable C'WlListener where
  sizeOf _    = fromIntegral $ [C.pure| int { sizeof(struct wl_listener) }|]
  alignment _ = fromIntegral $ [C.pure| int { alignof(struct wl_listener) }|]
  peek        = error "peek not implemented for C'WlListener"
  poke _ _    = error "poke not implemented for C'WlListener"

instance Storable C'WlSignal where
  sizeOf _    = fromIntegral $ [C.pure| int { sizeof(struct wl_signal) }|]
  alignment _ = fromIntegral $ [C.pure| int { alignof(struct wl_signal) }|]
  peek        = error "peek not implemented for C'WlSignal"
  poke _ _    = error "poke not implemented for C'WlSignal"

-- |FFI is a relation between C types marshalled from inline-C and C2HS
-- |(via hsroots and its dependencies).
class FFI inlinec chs | chs -> inlinec where
  toC2HS :: inlinec -> chs
  toInlineC :: chs -> inlinec

-- |Here we convert from inline-C to C2HS the explicit way (via data constructor pattern matching).
instance FFI (Ptr C'WlDisplay) DisplayServer where
  toC2HS ptrToWlDisplay = (DisplayServer ((castPtr ptrToWlDisplay) :: Ptr DisplayServer))
  toInlineC (DisplayServer ptrToDisplayServer) = (castPtr ptrToDisplayServer) :: Ptr C'WlDisplay

-- instance FFI (Ptr C'WlEventLoop) EventLoop where
--  toC2HS ptrToWlEventLoop = (EventLoop ((castPtr ptrToWlEventLoop) :: Ptr EventLoop))
--  toInlineC (EventLoop ptrToEventLoop) = (castPtr ptrToEventLoop) :: Ptr C'WlEventLoop

-- |Below we marshal via `coerce` (and assume that GHC knows what to do automatically).
-- |This seems justified since:
-- |"Any two nominally equal types are also representationally equal, and in addition, a newtype is
-- | representationally equal to its underlying type. For example, GHC knows that Int is
-- | representationally equal to Int, and if you have newtype WrappedInt = WrapInt Int,
-- | then GHC knows that Int is representationally equal to WrappedInt." -- /r/haskell
instance FFI (Ptr C'WlEventLoop) EventLoop where
 toC2HS = coerce
 toInlineC = coerce

-- |The rest of hsroot's types can be casted fairly straightforwardly via `castPtr`.
instance FFI (Ptr C'WlrCompositor) (Ptr WlrCompositor) where
  toC2HS ptrToWlrCompositor = (castPtr ptrToWlrCompositor) :: Ptr WlrCompositor
  toInlineC ptrToWlrCompositor = (castPtr ptrToWlrCompositor) :: Ptr C'WlrCompositor

instance FFI (Ptr C'WlrBackend) (Ptr Backend) where
  toC2HS ptrToWlrBackend = (castPtr ptrToWlrBackend) :: Ptr Backend
  toInlineC ptrToBackend = (castPtr ptrToBackend) :: Ptr C'WlrBackend

instance FFI (Ptr C'WlrSurface) (Ptr WlrSurface) where
  toC2HS ptrToWlrSurface = (castPtr ptrToWlrSurface) :: Ptr WlrSurface
  toInlineC ptrToWlrSurface = (castPtr ptrToWlrSurface) :: Ptr C'WlrSurface

instance FFI (Ptr C'WlrOutput) (Ptr WlrOutput) where
  toC2HS ptrToWlrOutput = (castPtr ptrToWlrOutput) :: Ptr WlrOutput
  toInlineC ptrToWlrOutput = (castPtr ptrToWlrOutput) :: Ptr C'WlrOutput

instance FFI (Ptr C'WlrRenderer) (Ptr Renderer) where
  toC2HS ptrToWlrRenderer = (castPtr ptrToWlrRenderer) :: Ptr Renderer
  toInlineC ptrRenderer = (castPtr ptrRenderer) :: Ptr C'WlrRenderer

instance FFI (Ptr C'WlrCursor) (Ptr WlrCursor) where
  toC2HS ptr = (castPtr ptr) :: Ptr WlrCursor
  toInlineC ptr = (castPtr ptr) :: Ptr C'WlrCursor

instance FFI (Ptr C'WlrXCursorManager) (Ptr WlrXCursorManager) where
  toC2HS ptr = (castPtr ptr) :: Ptr WlrXCursorManager
  toInlineC ptr = (castPtr ptr) :: Ptr C'WlrXCursorManager

instance FFI (Ptr C'WlrSeat) (Ptr WlrSeat) where
  toC2HS ptr = (castPtr ptr) :: Ptr WlrSeat
  toInlineC ptr = (castPtr ptr) :: Ptr C'WlrSeat

instance FFI (Ptr C'WlrOutputLayout) (Ptr WlrOutputLayout) where
  toC2HS = castPtr
  toInlineC = castPtr

instance FFI (Ptr C'WlrXdgShell) (Ptr WlrXdgShell) where
  toC2HS = castPtr
  toInlineC = castPtr

instance FFI (Ptr C'WlrKeyboard) (Ptr WlrKeyboard) where
  toC2HS = castPtr
  toInlineC = castPtr

-- instance FFI (Ptr C'WlrEventKeyboardKey) (EventKey) where
--   toC2HS = castPtr
--   toInlineC = castPtr

instance FFI (Ptr C'WlrXdgSurface) (Ptr WlrXdgSurface) where
  toC2HS = castPtr
  toInlineC = castPtr

instance FFI (Ptr C'WlrSeatPointerState) (Ptr WlrSeatPointerState) where
  toC2HS = castPtr
  toInlineC = castPtr

instance FFI (Ptr C'WlrSeatClient) (Ptr WlrSeatClient) where
  toC2HS = castPtr
  toInlineC = castPtr

instance FFI (Ptr C'WlSignal) (Ptr (WlSignal a)) where
  toC2HS = castPtr
  toInlineC = castPtr

instance FFI (Ptr C'WlrInputDevice) (Ptr InputDevice) where
  toC2HS = castPtr
  toInlineC = castPtr

-- |This function allows us to cast to HsRoot's `newtype WlListener a = WlListener (Ptr a -> IO ())`.
-- |AFAIK, there is no straightforward way to add these types to FFI.
toHsRootsListener :: Ptr C'WlListener -> IO (WlListener a)
toHsRootsListener ptrToWlListener = do
  cWlNotifyFuncT <- [C.exp| wl_notify_func_t { $(struct wl_listener *ptrToWlListener)->notify}|]
  let func = (\ptrToA -> do let ptrToVoid = castPtr ptrToA :: Ptr ()
                            $(C.peekFunPtr [t| Ptr C'WlListener -> Ptr () -> IO ()|]) cWlNotifyFuncT nullPtr ptrToVoid)
  return (WlListener func)

-- |Make sure to update the wl_listener->list argument after using this function (if needed in your FFI calls).
toInlineCListener :: WlListener a -> IO (Ptr C'WlListener)
toInlineCListener (WlListener wrappedFunc) = do
  let fullFunc = (\ptrListener ptrVoid -> do
                     let ptrA = (castPtr ptrVoid) :: Ptr a
                     (wrappedFunc ptrA))
  -- Below we construct a `struct wl_listener` (jamming an empty list into its wl_list field).
  r <- [C.block| struct wl_listener * {struct wl_listener * listener;
                                       struct wl_list empty_list;
                                       wl_list_init(&empty_list);
                                       listener->link = empty_list; //hack
                                       listener->notify = $fun:(void (*fullFunc)(struct wl_listener *, void *));
                                       return listener;
                                      }|]
  return r
