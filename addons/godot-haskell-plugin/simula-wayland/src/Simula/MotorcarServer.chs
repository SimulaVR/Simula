module Simula.MotorcarServer where

import Foreign
import Foreign.C

#include "motorcar-server-protocol.h"

import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.ForeignPtr as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp

{#import Simula.WaylandServer #}

{#enum motorcar_surface_clipping_mode as MotorcarSurfaceClippingMode {underscoreToCase} with prefix="MOTORCAR_SURFACE_CLIPPING_MODE" add prefix="Motorcar"
  deriving (Show, Eq, Ord) #}


type GetMotorcarSurfaceFunc = WlClient -> WlResource -> CUInt -> WlResource -> CUInt -> CUInt -> IO ()
foreign import ccall "wrapper" createGetMotorcarSurfaceFuncPtr :: GetMotorcarSurfaceFunc -> IO (FunPtr GetMotorcarSurfaceFunc)

type SetSize3DFunc = WlClient -> WlResource -> WlArray -> IO ()
foreign import ccall "wrapper" createSetSize3DFuncPtr :: SetSize3DFunc -> IO (FunPtr SetSize3DFunc)


{#fun motorcar_surface_send_transform_matrix {`WlResource', `WlArray'} -> `()'#}
{#fun motorcar_surface_send_request_size_3d {`WlResource', `WlArray'} -> `()'#}
{#fun motorcar_viewpoint_send_view_matrix {`WlResource', `WlArray'} -> `()'#}
{#fun motorcar_viewpoint_send_projection_matrix {`WlResource', `WlArray'} -> `()'#}
{#fun motorcar_viewpoint_send_view_port {`WlResource'
                                        , `CInt', `CInt', `CUInt', `CUInt'
                                        , `CInt', `CInt', `CUInt', `CUInt'} -> `()' #}
