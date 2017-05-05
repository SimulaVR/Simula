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
  _keyboardFocus :: IORef (Maybe (Some WaylandSurface))
  }

data Pointer = Pointer {
  _pointerLocalPosition :: IORef  (V2 Float),
  _pointerFocus :: IORef (Maybe (Some WaylandSurface)),
  _pointerCursorNode :: IORef (Maybe (Some WaylandSurfaceNode)),
  _pointerCursorHotspot :: IORef (Maybe (V2 Int))
  }

data BaseSeat = BaseSeat {
  _baseSeatKeyboard :: IORef Keyboard,
  _baseSeatPointer :: IORef Pointer
  } deriving Eq

makeLenses ''Keyboard
makeLenses ''Pointer
makeClassy ''BaseSeat


class (Eq a, Typeable a) => Seat a where
  seatKeyboard :: a -> IO Keyboard
  default seatKeyboard :: HasBaseSeat a => a -> IO Keyboard
  seatKeyboard = views baseSeatKeyboard readIORef
  
  setSeatKeyboard :: a -> Keyboard -> IO ()
  default setSeatKeyboard :: HasBaseSeat a => a -> Keyboard -> IO ()
  setSeatKeyboard = views baseSeatKeyboard writeIORef

  setSeatKeyboardFocus :: a -> Maybe (Some WaylandSurface) -> IO ()
  setSeatKeyboardFocus this ws = do
    kb <- seatKeyboard this
    writeIORef (_keyboardFocus kb) ws
  
  seatPointer :: a -> IO Pointer
  default seatPointer :: HasBaseSeat a => a -> IO Pointer  
  seatPointer = views baseSeatPointer readIORef
  
  setSeatPointer :: a -> Pointer -> IO ()
  default setSeatPointer :: HasBaseSeat a => a -> Pointer -> IO ()
  setSeatPointer = views baseSeatPointer writeIORef

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


newKeyboard :: IO Keyboard
newKeyboard = Keyboard <$> newIORef Nothing

newPointer :: IO Pointer
newPointer = Pointer
             <$> newIORef (V2 0 0)
             <*> newIORef Nothing
             <*> newIORef Nothing
             <*> newIORef Nothing

newBaseSeat :: IO BaseSeat
newBaseSeat = do
  kb <- newKeyboard
  ptr <- newPointer
  BaseSeat <$> newIORef kb <*> newIORef ptr
