module Simula.Compositor.Events where

import Data.Word

newtype Vec2 = Vec2 (Double, Double)
  deriving (Show, Eq, Ord)

data MouseButton = Left | Middle | Right
  deriving (Show, Eq, Ord, Enum)

data Orientation = Horizontal | Vertical
  deriving (Show, Eq, Ord, Enum)

data MouseEvent
  = MouseMotion  !Vec2
  | MouseEnter   !Vec2
  | MouseLeave   !Vec2
  | MousePress   !Vec2 !MouseButton
  | MouseRelease !Vec2 !MouseButton
  | MouseWheel   !Vec2 !Orientation Int
  deriving (Show, Eq, Ord)

data KeyboardEvent
  = KeyboardPress Word32
  | KeyboardRelease Word32
  deriving (Show, Eq, Ord)
