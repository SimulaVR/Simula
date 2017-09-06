module Simula.Compositor.Wayland.Input where

import Control.Lens
import Control.Monad
import Control.Concurrent.MVar
import Data.Typeable
import Linear

import Simula.Compositor.Types
import {-# SOURCE #-} Simula.Compositor.SceneGraph.Wayland
import Simula.Compositor.Wayland.Output
import Simula.Compositor.Utils

data Keyboard = Keyboard {
  _keyboardFocus :: MVar (Maybe (Some WaylandSurface))
  }

data Pointer = Pointer {
  _pointerGlobalPosition :: MVar (V2 Float),
  _pointerLocalPosition :: MVar (V2 Float),
  _pointerFocus :: MVar (Maybe (Some WaylandSurface)),
  _pointerCursorNode :: MVar (Maybe (Some WaylandSurfaceNode)),
  _pointerCursorHotspot :: MVar (Maybe (V2 Int))
  }

data BaseSeat = BaseSeat {
  _baseSeatKeyboard :: MVar Keyboard,
  _baseSeatPointer :: MVar Pointer
  } deriving Eq

makeLenses ''Keyboard
makeLenses ''Pointer
makeClassy ''BaseSeat


class (Eq a, Typeable a) => Seat a where
  seatKeyboard :: a -> IO Keyboard
  default seatKeyboard :: HasBaseSeat a => a -> IO Keyboard
  seatKeyboard = views baseSeatKeyboard readMVar
  
  setSeatKeyboard :: a -> Keyboard -> IO ()
  default setSeatKeyboard :: HasBaseSeat a => a -> Keyboard -> IO ()
  setSeatKeyboard = views baseSeatKeyboard writeMVar

  setSeatKeyboardFocus :: a -> Maybe (Some WaylandSurface) -> IO ()
  setSeatKeyboardFocus this ws = do
    kb <- seatKeyboard this
    writeMVar (_keyboardFocus kb) ws
  
  seatPointer :: a -> IO Pointer
  default seatPointer :: HasBaseSeat a => a -> IO Pointer  
  seatPointer = views baseSeatPointer readMVar
  
  setSeatPointer :: a -> Pointer -> IO ()
  default setSeatPointer :: HasBaseSeat a => a -> Pointer -> IO ()
  setSeatPointer = views baseSeatPointer writeMVar

  setSeatPointerFocus :: WaylandSurface ws => a -> ws -> V2 Float -> IO ()
  setSeatPointerFocus this ws pos = do
    p <- seatPointer this
    writeMVar (_pointerFocus p) $ Just (Some ws)
    writeMVar (_pointerLocalPosition p) pos

    -- DIFF from C++
    setSeatKeyboardFocus this (Just (Some ws))

instance Eq (Some Seat) where
  Some a == Some b = case cast b of
    Just b -> a == b
    _ -> False


seatEnsureKeyboardFocusIsValid :: Seat a => WaylandSurface ws => a -> ws -> Maybe (Some WaylandSurface) -> IO ()
seatEnsureKeyboardFocusIsValid this old next = do
  kb <- seatKeyboard this
  kbFocus <- readMVar $ _keyboardFocus kb
  when (kbFocus == Just (Some old) || kbFocus == Nothing) $ setSeatKeyboardFocus this next


newKeyboard :: IO Keyboard
newKeyboard = Keyboard <$> newMVar Nothing

newPointer :: IO Pointer
newPointer = Pointer
             <$> newMVar (V2 0 0)
             <*> newMVar (V2 0 0)
             <*> newMVar Nothing
             <*> newMVar Nothing
             <*> newMVar Nothing

newBaseSeat :: IO BaseSeat
newBaseSeat = do
  kb <- newKeyboard
  ptr <- newPointer
  BaseSeat <$> newMVar kb <*> newMVar ptr
