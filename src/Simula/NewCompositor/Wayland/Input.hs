module Simula.NewCompositor.Wayland.Input where

import Control.Monad
import Data.IORef
import Linear

import Simula.NewCompositor.Types
import Simula.NewCompositor.Wayland.Output

data WaylandSurfaceNode


data Keyboard = Keyboard {
  _keyboardSeat :: Some Seat,
  _keyboardFocus :: IORef (Maybe (Some WaylandSurface))
  }

data Pointer = Pointer {
  _pointerSeat :: Some Seat,
  _pointerLocalPosition :: IORef (Maybe (V2 Float)),
  _pointerFocus :: IORef (Maybe (Some WaylandSurface)),
  _pointerCursorNode :: IORef (Maybe WaylandSurfaceNode),
  _pointerCursorHotspot :: IORef (Maybe (V2 Int))
  }
  
class Seat a where
  seatKeyboard :: a -> IO Keyboard
  setSeatKeyboard :: a -> Keyboard -> IO ()

  setSeatKeyboardFocus :: WaylandSurface ws => a -> ws -> IO ()
  setSeatKeyboardFocus this ws = do
    kb <- seatKeyboard this
    writeIORef (_keyboardFocus kb) $ Just (Some ws)
  
  seatPointer :: a -> IO Pointer
  setSeatPointer :: a -> Pointer -> IO ()

  setSeatPointerFocus :: WaylandSurface ws => a -> ws -> V2 Float -> IO ()
  setSeatPointerFocus this ws pos = do
    p <- seatPointer this
    writeIORef (_pointerFocus p) $ Just (Some ws)
    writeIORef (_pointerLocalPosition p) (Just pos)

    -- DIFF from C++
    setSeatKeyboardFocus this ws

instance Seat (Some Seat) where
  seatKeyboard (Some x) = seatKeyboard x
  setSeatKeyboard (Some x) = setSeatKeyboard x
  setSeatKeyboardFocus (Some x) = setSeatKeyboardFocus x
  seatPointer (Some x) = seatPointer x
  setSeatPointer (Some x) = setSeatPointer x
  setSeatPointerFocus (Some x) = setSeatPointerFocus x


seatEnsureKeyboardFocusIsValid :: Seat a => WaylandSurface ws => a -> ws -> ws -> IO ()
seatEnsureKeyboardFocusIsValid this old next = do
  kb <- seatKeyboard this
  kbFocus <- readIORef $ _keyboardFocus kb
  when (kbFocus == Just (Some old) || kbFocus == Nothing) $ setSeatKeyboardFocus this next
