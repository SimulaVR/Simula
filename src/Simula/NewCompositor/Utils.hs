module Simula.NewCompositor.Utils where

import Foreign
import Foreign.C

import qualified Language.C.Inline as C

import Linear

import Simula.MotorcarServer
import Simula.WaylandServer

C.context waylandCtx
C.include "motorcar-server-protocol.h"

radians :: Floating a => a -> a
radians = (* (pi/180))

translate :: Num a => V3 a -> M44 a
translate = mkTransformationMat identity

scale :: Num a => V3 a -> M44 a
scale v = fmap (liftI2 (*) (point v)) identity

motorcarShellInterface :: IO WlInterface
motorcarShellInterface = WlInterface <$> [C.exp| struct wl_interface* { &motorcar_shell_interface } |]

motorcarShellVersion :: IO Int
motorcarShellVersion = fromIntegral <$>  [C.exp| int { motorcar_shell_interface.version } |]

motorcarSurfaceInterface :: IO WlInterface
motorcarSurfaceInterface = WlInterface <$> [C.exp| struct wl_interface* { &motorcar_surface_interface } |]

motorcarSurfaceVersion :: IO Int
motorcarSurfaceVersion = fromIntegral <$>  [C.exp| int { motorcar_surface_interface.version } |]
