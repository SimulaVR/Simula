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

{#pointer *wl_array as WlArray_ foreign finalizer wl_array_release newtype #}
{#pointer *wl_resource as WlResource newtype #}
{#pointer *wl_client as WlClient newtype #}
{#pointer *wl_display as WlDisplay newtype #}
{#pointer *wl_interface as WlInterface newtype #}
{#pointer *wl_global as WlGlobal foreign finalizer wl_global_destroy newtype #}

deriving instance Eq WlGlobal

{#fun wl_array_init { `WlArray_' } -> `()' #}

newtype WlArray a = WlArray WlArray_

newWlArray :: IO (WlArray a)
newWlArray = do
  ptr <- mallocBytes {#sizeof wl_array#} >>= newForeignPtr wl_array_release
  let array = WlArray_ ptr
  wl_array_init array
  return $ WlArray array

{#fun wl_array_add { `WlArray_', `CSize' } -> `Ptr ()' #}

wlArrayAdd :: Int -> WlArray a -> IO ()
wlArrayAdd size (WlArray arr) = do
  res <- wl_array_add arr (fromIntegral size)
  if (res == nullPtr)
    then ioError $ userError "wl_array_add returned NULL"
    else return ()

wlArrayOverwrite :: Storable a => [a] -> WlArray a -> IO ()
wlArrayOverwrite xs arr@(WlArray arr_)
  = withArrayLen xs $ \len xsPtr -> withWlArray_ arr_ $ \arrPtr -> do
    size <- {#get wl_array->size#} arrPtr
    let diff = (len - fromIntegral size)
    when (diff > 0) $ wlArrayAdd diff arr
    datPtr <- castPtr <$> {#get wl_array->data#} arrPtr
    copyArray datPtr xsPtr len


type GlobalBindFunc = WlClient -> Ptr () -> CUInt -> CUInt -> IO ()

foreign import ccall "wrapper" createGlobalBindFuncPtr :: GlobalBindFunc -> IO (FunPtr GlobalBindFunc)

{#fun wl_global_create {`WlDisplay', `WlInterface', `Int', `Ptr ()', id `FunPtr GlobalBindFunc'} -> `WlGlobal' #}

{#fun wl_resource_create {`WlClient', `WlInterface', `Int', `Word32'} -> `WlResource' #}

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
