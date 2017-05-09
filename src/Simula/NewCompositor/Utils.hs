module Simula.NewCompositor.Utils where

import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Monad

import Foreign hiding (void)
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

writeMVar :: MVar a -> a -> IO ()
writeMVar mv x = void $ swapMVar mv x

modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' mv f = modifyMVar_ mv (\x -> f <$> evaluate x)

motorcarShellInterface :: IO WlInterface
motorcarShellInterface = WlInterface <$> [C.exp| struct wl_interface* { &motorcar_shell_interface } |]

motorcarShellVersion :: IO Int
motorcarShellVersion = fromIntegral <$>  [C.exp| int { motorcar_shell_interface.version } |]

motorcarSurfaceInterface :: IO WlInterface
motorcarSurfaceInterface = WlInterface <$> [C.exp| struct wl_interface* { &motorcar_surface_interface } |]

motorcarSurfaceVersion :: IO Int
motorcarSurfaceVersion = fromIntegral <$>  [C.exp| int { motorcar_surface_interface.version } |]

motorcarViewpointInterface :: IO WlInterface
motorcarViewpointInterface = WlInterface <$> [C.exp| struct wl_interface* { &motorcar_viewpoint_interface } |]

motorcarViewpointVersion :: IO Int
motorcarViewpointVersion = fromIntegral <$>  [C.exp| int { motorcar_viewpoint_interface.version } |]
