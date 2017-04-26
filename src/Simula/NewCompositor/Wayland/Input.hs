module Simula.NewCompositor.Wayland.Input where

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Typeable
import Linear

import Simula.NewCompositor.Types
import {-# SOURCE #-} Simula.NewCompositor.SceneGraph.Wayland
import Simula.NewCompositor.Wayland.Output


data Keyboard = Keyboard {
  _keyboardSeat :: Some Seat,
  _keyboardFocus :: IORef (Maybe (Some WaylandSurface))
  }

data Pointer = Pointer {
  _pointerSeat :: Some Seat,
  _pointerLocalPosition :: IORef (V2 Float),
  _pointerFocus :: IORef (Maybe (Some WaylandSurface)),
  _pointerCursorNode :: IORef (Maybe (Some WaylandSurfaceNode)),
  _pointerCursorHotspot :: IORef (Maybe (V2 Int))
  }
  
class (Eq a, Typeable a) => Seat a where
  seatKeyboard :: a -> IO Keyboard
  setSeatKeyboard :: a -> Keyboard -> IO ()

  setSeatKeyboardFocus :: a -> Maybe (Some WaylandSurface) -> IO ()
  setSeatKeyboardFocus this ws = do
    kb <- seatKeyboard this
    writeIORef (_keyboardFocus kb) ws
  
  seatPointer :: a -> IO Pointer
  setSeatPointer :: a -> Pointer -> IO ()

  setSeatPointerFocus :: WaylandSurface ws => a -> ws -> V2 Float -> IO ()
  setSeatPointerFocus this ws pos = do
    p <- seatPointer this
    writeIORef (_pointerFocus p) $ Just (Some ws)
    writeIORef (_pointerLocalPosition p) pos

    -- DIFF from C++
    setSeatKeyboardFocus this (Just (Some ws))

instance Eq (Some Seat) where
  Some a == Some b = case cast b of
    Just b -> a == b
    _ -> False

seatEnsureKeyboardFocusIsValid :: Seat a => WaylandSurface ws => a -> ws -> Maybe (Some WaylandSurface) -> IO ()
seatEnsureKeyboardFocusIsValid this old next = do
  kb <- seatKeyboard this
  kbFocus <- readIORef $ _keyboardFocus kb
  when (kbFocus == Just (Some old) || kbFocus == Nothing) $ setSeatKeyboardFocus this next

makeLenses ''Keyboard
makeLenses ''Pointer
