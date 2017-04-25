module Simula.MotorcarServer where

import Foreign
import Foreign.C

import Simula.WaylandServer

#include "motorcar-server-protocol.h"

{#context lib="libmotorcar-server"#}

{#enum motorcar_surface_clipping_mode as MotorcarSurfaceClippingMode {underscoreToCase} with prefix="MOTORCAR_SURFACE_CLIPPING_MODE" add prefix="Motorcar"
  deriving (Show, Eq, Ord) #}


type GetMotorcarSurfaceFunc = WlClient -> WlResource -> CUInt -> WlResource -> CUInt -> CUInt -> IO ()
foreign import ccall "wrapper" createGetMotorcarSurfaceFuncPtr :: GetMotorcarSurfaceFunc -> IO (FunPtr GetMotorcarSurfaceFunc)
