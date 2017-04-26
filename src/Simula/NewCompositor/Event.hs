module Simula.NewCompositor.Event where

import Data.Word
import Linear

import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Types

data InputEvent
  = MouseEvent (Some Seat) MouseEvent
  | KeyboardEvent (Some Seat) KeyboardEvent

data MouseButton = Left | Middle | Right
  deriving (Show, Eq, Ord, Enum)

data MouseEvent
  = MouseButtonPress MouseButton (V2 Float)
  | MouseButtonRelease MouseButton (V2 Float)
  | MouseMove (V2 Float)
  | MouseEnter (V2 Float)
  | MouseLeave (V2 Float)
  deriving (Show, Eq, Ord)

--TOOD strongly typed keycodes
data KeyboardEvent
  = KeyboardPress Word32
  | KeyboardRelease Word32
  deriving (Show, Eq, Ord)

