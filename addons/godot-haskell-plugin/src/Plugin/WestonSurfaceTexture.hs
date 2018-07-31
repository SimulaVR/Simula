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

import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T
import           Linear
import           Plugin.Imports

import Godot.Gdnative.Internal.Api
import           Godot.Gdnative.Types        (GodotFFI, LibType, TypeOf)
import qualified Godot.Methods               as G

import qualified Godot.Core.GodotImage as Image

import Foreign

data GodotWestonSurfaceTexture = GodotWestonSurfaceTexture
  { _gwsObj      :: GodotObject
  , _gwsSurface :: TVar WestonSurface
  , _gwsImage :: TVar GodotImage
  }

instance GodotClass GodotWestonSurfaceTexture where
  godotClassName = "WestonSurfaceTexture"

instance ClassExport GodotWestonSurfaceTexture where
  classInit obj = GodotWestonSurfaceTexture obj <$> atomically (newTVar undefined) <*> atomically (newTVar undefined)
  --classInit obj = atomically $
    -- CubeMaker obj <$> newTVar [] <*> newTVar ""
  classExtends = "ImageTexture"
  classMethods = []

instance HasBaseClass GodotWestonSurfaceTexture where
  type BaseClass GodotWestonSurfaceTexture = GodotImageTexture         
  super (GodotWestonSurfaceTexture obj _ _) = GodotImageTexture obj

getVisualServer :: IO GodotVisualServer
getVisualServer = GodotVisualServer <$> getSingleton "VisualServer"

getResourceLoader :: IO Godot_ResourceLoader
getResourceLoader = Godot_ResourceLoader <$> getSingleton "ResourceLoader"

newGodotWestonSurfaceTexture :: IO GodotWestonSurfaceTexture
newGodotWestonSurfaceTexture = do
  rl <- getResourceLoader
  url <- toLowLevel "res://addons/godot-haskell-plugin/WestonSurfaceTexture.gdns"
  typeHint <- toLowLevel ""
  (GodotResource obj) <- G.load rl url typeHint False
  let ns = GodotNativeScript obj
  ret <- G.new ns []
  classInit ret

setWestonSurface :: GodotWestonSurfaceTexture -> WestonSurface -> IO ()
setWestonSurface gws ws = do 
  atomically $ writeTVar (_gwsSurface gws) ws
  updateWestonSurfaceTexture gws

updateWestonSurfaceTexture :: GodotWestonSurfaceTexture -> IO ()
updateWestonSurfaceTexture gws = do
  ws <- atomically $ readTVar (_gwsSurface gws)
  visualServer <- getVisualServer

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

      byteArray <- godot_pool_byte_array_new
      godot_pool_byte_array_resize byteArray size
      writeAccess <- godot_pool_byte_array_write byteArray
      writePtr <- godot_pool_byte_array_write_access_ptr writeAccess

      img <- GodotImage <$> mkClassInstance "Image"

      wl_shm_buffer_begin_access shmbuf
      copyBytes writePtr (castPtr dt) size
      wl_shm_buffer_end_access shmbuf
      godot_pool_byte_array_write_access_destroy writeAccess -- TODO helper function

      G.create_from_data img width height False fmt byteArray
      godot_pool_byte_array_destroy byteArray -- same

      atomically $ writeTVar (_gwsImage gws) img
      name <- toLowLevel "create_from_image"
      G.call_deferred gws name [toVariant img, toVariant (7 :: Int)]
--      str <- toLowLevel  "test.png" :: IO GodotString
--      G.save_png img str
      return ()

  where
    asGodotFormat shmFmt = case shmFmt of
      WlShmFormatArgb8888 -> Image.FORMAT_RGBA8
      _ -> error $ "Unknown SHM format " ++ show shmFmt
  