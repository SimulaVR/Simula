module Simula.Compositor.Types where

import Foreign

newtype Display = Display (Ptr Display)
  deriving (Show, Eq)

newtype Compositor = Compositor (Ptr Compositor)
  deriving (Show, Eq)

newtype Scene = Scene (Ptr Scene)
  deriving (Show, Eq)

newtype WindowManager = WindowManager (Ptr WindowManager)
  deriving (Show, Eq)

newtype OpenGLContext = OpenGLContext (Ptr OpenGLContext)
  deriving (Show, Eq)

newtype Skeleton = Skeleton (Ptr Skeleton)
  deriving (Show, Eq)

newtype Seat = Seat (Ptr Seat)
  deriving (Show, Eq)

newtype ViewPoint = ViewPoint (Ptr ViewPoint)
  deriving (Show, Eq)

--GLM

newtype Vec2 = Vec2 (Ptr Vec2)
  deriving (Show, Eq)

newtype Vec3 = Vec3 (Ptr Vec3)
  deriving (Show, Eq)

newtype Mat4 = Mat4 (Ptr Mat4)
  deriving (Show, Eq)
