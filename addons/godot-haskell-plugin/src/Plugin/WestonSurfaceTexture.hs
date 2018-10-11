{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugin.WestonSurfaceTexture 
  ( GodotWestonSurfaceTexture(..)
  , newGodotWestonSurfaceTexture, setWestonSurface
  , updateWestonSurfaceTexture) where

import Simula.WaylandServer
import Simula.Weston

import Control.Monad
import Data.Coerce

import           Plugin.Imports
import           Godot.Extra.Register

import qualified Godot.Methods               as G
import Godot.Gdnative.Internal.Api

import qualified Godot.Core.GodotImage as Image

import Foreign


data GodotWestonSurfaceTexture = GodotWestonSurfaceTexture
  { _gwstObj      :: GodotObject
  , _gwstSurface :: TVar WestonSurface
  , _gwstView :: TVar WestonView
  , _gwstImage :: TVar GodotImage
  , _gwstImageData :: TVar GodotPoolByteArray
  }

instance GodotClass GodotWestonSurfaceTexture where
  godotClassName = "WestonSurfaceTexture"

instance ClassExport GodotWestonSurfaceTexture where
  classInit obj = do
    img <- unsafeInstance GodotImage "Image"
    imgdt <- godot_pool_byte_array_new
    GodotWestonSurfaceTexture obj
      <$> atomically (newTVar undefined)
      <*> atomically (newTVar undefined)
      <*> atomically (newTVar img)
      <*> atomically (newTVar imgdt)
  classExtends = "ImageTexture"
  classMethods = []

instance HasBaseClass GodotWestonSurfaceTexture where
  type BaseClass GodotWestonSurfaceTexture = GodotImageTexture         
  super (GodotWestonSurfaceTexture obj _ _ _ _) = GodotImageTexture obj

newGodotWestonSurfaceTexture :: IO GodotWestonSurfaceTexture
newGodotWestonSurfaceTexture = do
  ret <- unsafeNewNS id "Object" [] "res://addons/godot-haskell-plugin/WestonSurfaceTexture.gdns"
  objPtr <- godot_nativescript_get_userdata ret
  deRefStablePtr $ castPtrToStablePtr objPtr

setWestonSurface :: GodotWestonSurfaceTexture -> WestonSurface -> WestonView -> IO ()
setWestonSurface gws ws view = do 
  atomically $ writeTVar (_gwstSurface gws) ws
  atomically $ writeTVar (_gwstView gws) view
  updateWestonSurfaceTexture gws

updateWestonSurfaceTexture :: GodotWestonSurfaceTexture -> IO ()
updateWestonSurfaceTexture gws = do
  ws <- atomically $ readTVar (_gwstSurface gws)

  buffer <- westonSurfaceBuffer ws
  when (coerce buffer /= nullPtr) $ do
    res <- westonBufferResource buffer
    shmbuf <- wl_shm_buffer_get res
  
    when (coerce shmbuf /= nullPtr) $ do
      dt <- wl_shm_buffer_get_data shmbuf
      width <- wl_shm_buffer_get_width shmbuf
      height <- wl_shm_buffer_get_height shmbuf
      stride <- wl_shm_buffer_get_stride shmbuf
      shmFmt <- wl_shm_buffer_get_format shmbuf
      let fmt = asGodotFormat shmFmt
      let size :: Integral a => a 
          size = fromIntegral (stride * height)
    
      img <- atomically $ readTVar (_gwstImage gws)
      byteArray <- godot_pool_byte_array_new
      cursize <- godot_pool_byte_array_size byteArray
      when (cursize /= size) $ godot_pool_byte_array_resize byteArray size

      writeAccess <- godot_pool_byte_array_write byteArray
      writePtr <- godot_pool_byte_array_write_access_ptr writeAccess

      wl_shm_buffer_begin_access shmbuf
      copyBytes writePtr (castPtr dt) size
      wl_shm_buffer_end_access shmbuf
      godot_pool_byte_array_write_access_destroy writeAccess -- TODO helper function

      G.create_from_data img width height False fmt byteArray

      G.create_from_image gws img (7 :: Int)
      godot_pool_byte_array_destroy byteArray
      
      return ()

  where
    asGodotFormat shmFmt = case shmFmt of
      WlShmFormatArgb8888 -> Image.FORMAT_RGBA8
      _ -> error $ "Unknown SHM format " ++ show shmFmt
  
