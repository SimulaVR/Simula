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



{#pointer *wl_client as WlClient newtype #}
{#pointer *wl_interface as WlInterface newtype #}
{#pointer *wl_surface as WlSurface newtype#}


{#pointer *wl_array as WlArray newtype #}
deriving instance Eq WlArray

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



{#pointer *wl_display as WlDisplay newtype #}
deriving instance Eq WlDisplay
{#fun wl_display_create {} -> `WlDisplay' #}
{#fun wl_display_add_socket_auto {`WlDisplay'} -> `String'#}
{#fun wl_display_run {`WlDisplay'} -> `()' #}



{#pointer *wl_global as WlGlobal newtype #}
deriving instance Eq WlGlobal

type GlobalBindFunc = WlClient -> Ptr () -> CUInt -> CUInt -> IO ()
foreign import ccall "wrapper" createGlobalBindFuncPtr :: GlobalBindFunc -> IO (FunPtr GlobalBindFunc)
{#fun wl_global_create {`WlDisplay', `WlInterface', `Int', `Ptr ()', id `FunPtr GlobalBindFunc'} -> `WlGlobal' #}



{#pointer *wl_resource as WlResource newtype #}
deriving instance Eq WlResource

{#fun wl_resource_create {`WlClient', `WlInterface', `Int', `Word32'} -> `WlResource' #}
{#fun wl_resource_destroy { `WlResource' } -> `()' #}
type ResourceDestroyFunc = WlResource -> IO ()
foreign import ccall "wrapper" createResourceDestroyFuncPtr :: ResourceDestroyFunc -> IO (FunPtr ResourceDestroyFunc)
{#fun wl_resource_set_implementation {`WlResource', `Ptr ()', `Ptr ()', id `FunPtr ResourceDestroyFunc'} -> `()' #}

wlResourceData :: WlResource -> IO (Ptr ())
wlResourceData = {# get wl_resource->data #}




newtype WlList a = WlList {toWlListPtr :: WlListPtr}
  deriving Eq
{#pointer *wl_list as WlListPtr #}

wlListNext :: WlList a -> IO (WlList a)
wlListNext = {#get wl_list->next#} . toWlListPtr >=> (return . WlList)

class WlListElement a where
  -- |Offset to the link element
  linkOffset :: proxy a -> Int

wlListData :: WlListElement a => WlList a -> Ptr a
wlListData list = let ptr = toWlListPtr list
                  in castPtr $ plusPtr ptr (negate $ linkOffset list)

wlListAll :: WlListElement a => WlList a -> IO [Ptr a]
wlListAll init = go init
  where
    go curr = do
      next <- wlListNext curr
      if next == init
        then return []
        else do
          let ptr = wlListData curr
          ptrs <- go next
          return (ptr:ptrs)
          




newtype WlListener a = WlListener {toWlListenerPtr :: WlListenerPtr}
{#pointer *wl_listener as WlListenerPtr #}

type NotifyFunc a = Ptr (WlListener a) -> Ptr () -> IO ()
foreign import ccall "wrapper" createNotifyFuncPtr :: NotifyFunc a -> IO (FunPtr (NotifyFunc a))

instance WlListElement (WlListener a) where
  linkOffset _ = {#offsetof wl_listener->link#}

withWlListener :: NotifyFunc a -> (WlListener a -> IO b) -> IO b
withWlListener nf act = do
  nfPtr <- createNotifyFuncPtr nf
  allocaBytes {#sizeof wl_listener#} $ \ptr -> do
    {#set wl_listener->notify#} ptr (castFunPtr nfPtr)
    act (WlListener ptr)
  

class WlListenerContainer a where
  -- |Offset to the relevant listener
  listenerOffset :: proxy a -> Int


{#pointer *wl_signal as WlSignal newtype#}
{#fun wl_signal_add {`WlSignal', `WlListenerPtr'} -> `()'#}

addListenerToSignal :: WlListenerContainer a => WlSignal -> (Ptr a -> Ptr () -> IO ()) -> IO ()
addListenerToSignal sig nf' = withWlListener nf $ \(WlListener ptr) -> wl_signal_add sig ptr
  where
    nf = nf' . castPtr


waylandCtx :: C.Context
waylandCtx = C.baseCtx <> mempty { C.ctxTypesTable = M.fromList [
                                     (C.Struct "wl_interface", [t| WlInterface |])
                                     ]
                                 }
