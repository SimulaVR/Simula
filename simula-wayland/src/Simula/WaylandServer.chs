module Simula.WaylandServer where

import Control.Monad
import qualified Data.Map as M
import Data.Monoid
import Data.Word
import Foreign
import Foreign.C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

#include "wayland-server.h"

{#context lib="libwayland-server"#}

{#typedef size_t CSize#}

{#pointer *wl_array as WlArray  newtype #}
{#pointer *wl_resource as WlResource newtype #}
{#pointer *wl_client as WlClient newtype #}
{#pointer *wl_display as WlDisplay newtype #}
{#pointer *wl_interface as WlInterface newtype #}
{#pointer *wl_global as WlGlobal newtype #}

deriving instance Eq WlArray
deriving instance Eq WlDisplay
deriving instance Eq WlGlobal
deriving instance Eq WlResource


{#fun wl_array_init { `WlArray' } -> `()' #}
{#fun wl_array_release { `WlArray' } -> `()' #}

newWlArray :: IO WlArray
newWlArray = do
  ptr <- mallocBytes {#sizeof wl_array#}
  let array = WlArray ptr
  wl_array_init array
  return array

{#fun wl_array_add { `WlArray', `CSize' } -> `Ptr ()' #}

wlArrayAdd :: Int -> WlArray -> IO ()
wlArrayAdd size arr = do
  res <- wl_array_add arr (fromIntegral size)
  if (res == nullPtr)
    then ioError $ userError "wl_array_add returned NULL"
    else return ()

wlArrayOverwrite :: Storable a => WlArray -> a -> IO ()
wlArrayOverwrite arr x
  = with x $ \xPtr -> do
    size <- wlArraySize arr
    let diff = (sizeOf x - fromIntegral size)
    when (diff > 0) $ wlArrayAdd diff arr
    datPtr <- castPtr <$> wlArrayData arr
    copyBytes datPtr xPtr (sizeOf x)

wlArrayData :: WlArray -> IO (Ptr a)
wlArrayData = {#get wl_array->data#} >=> (return . castPtr)

wlArraySize :: WlArray -> IO CULong
wlArraySize = {#get wl_array->size#} 
  

type GlobalBindFunc = WlClient -> Ptr () -> CUInt -> CUInt -> IO ()

foreign import ccall "wrapper" createGlobalBindFuncPtr :: GlobalBindFunc -> IO (FunPtr GlobalBindFunc)

{#fun wl_global_create {`WlDisplay', `WlInterface', `Int', `Ptr ()', id `FunPtr GlobalBindFunc'} -> `WlGlobal' #}

{#fun wl_resource_create {`WlClient', `WlInterface', `Int', `Word32'} -> `WlResource' #}
{#fun wl_resource_destroy { `WlResource' } -> `()' #}

type ResourceDestroyFunc = WlResource -> IO ()
foreign import ccall "wrapper" createResourceDestroyFuncPtr :: ResourceDestroyFunc -> IO (FunPtr ResourceDestroyFunc)

{#fun wl_resource_set_implementation {`WlResource', `Ptr ()', `Ptr ()', id `FunPtr ResourceDestroyFunc'} -> `()' #}

wlResourceData :: WlResource -> IO (Ptr ())
wlResourceData = {# get wl_resource->data #} 

waylandCtx :: C.Context
waylandCtx = C.baseCtx <> mempty { C.ctxTypesTable = M.fromList [
                                     (C.Struct "wl_interface", [t| WlInterface |])
                                     ]
                                 }
