{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugin.SimulaViewTexture where

import Debug.C

import Control.Monad
import Data.Coerce

import           Plugin.Imports
import           Godot.Extra.Register

import qualified Godot.Methods               as G
import Godot.Gdnative.Internal.Api

import qualified Godot.Core.GodotImage as Image

import Foreign


data GodotSimulaViewTexture = GodotSimulaViewTexture
  { _gwstObj      :: GodotObject
  , _gwstSurface :: TVar (Ptr C'WlrSurface)
  , _gwstView :: TVar (Ptr C'WlrView)
  , _gwstImage :: TVar GodotImage
  , _gwstImageData :: TVar GodotPoolByteArray
  }

instance GodotClass GodotSimulaViewTexture where
  godotClassName = "SimulaViewTexture"

instance ClassExport GodotSimulaViewTexture where
  classInit obj = do
    img <- unsafeInstance GodotImage "Image"
    imgdt <- godot_pool_byte_array_new
    GodotSimulaViewTexture obj
      <$> atomically (newTVar undefined)
      <*> atomically (newTVar undefined)
      <*> atomically (newTVar img)
      <*> atomically (newTVar imgdt)
  classExtends = "ImageTexture"
  classMethods = []

instance HasBaseClass GodotSimulaViewTexture where
  type BaseClass GodotSimulaViewTexture = GodotImageTexture         
  super (GodotSimulaViewTexture obj _ _ _ _) = GodotImageTexture obj

newGodotSimulaViewTexture :: IO GodotSimulaViewTexture
newGodotSimulaViewTexture = do
  ret <- unsafeNewNS id "Object" [] "res://addons/godot-haskell-plugin/SimulaViewTexture.gdns"
  objPtr <- godot_nativescript_get_userdata ret
  deRefStablePtr $ castPtrToStablePtr objPtr

setWlrSurface :: GodotSimulaViewTexture -> (Ptr C'WlrSurface) -> (Ptr C'WlrView) -> IO ()
setWlrSurface gws ws view = do 
  atomically $ writeTVar (_gwstSurface gws) ws
  atomically $ writeTVar (_gwstView gws) view
  updateSimulaViewTexture gws

updateSimulaViewTexture :: GodotSimulaViewTexture -> IO ()
updateSimulaViewTexture gws = do
  putStrLn "updateSimulaViewTexture not yet implemented."
  {-
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

      allocaArray size $ \arrPtr -> do
        wl_shm_buffer_begin_access shmbuf
        copyBytes arrPtr (castPtr dt) size
        wl_shm_buffer_end_access shmbuf
        
        let arrPtr32 = castPtr arrPtr :: Ptr Word32
        forM_ [0..size `div` 4] $ \n -> do
          let ptr = advancePtr arrPtr32 n
          elem <- peek ptr
          -- HACKHACK
          let [b3,b2,b1,b0] = map (\n -> shiftR elem (n*8) .&. 0xff) [0..3]
          let result = foldr (\n res -> shiftL res 8 .|. n) 0 [b1,b2,b3,b0]

          poke ptr result

        copyBytes writePtr arrPtr size
      godot_pool_byte_array_write_access_destroy writeAccess -- TODO helper function

      G.create_from_data img width height False fmt byteArray
      G.create_from_image gws img (7 .|. 16 :: Int)
      godot_pool_byte_array_destroy byteArray
     -}

  where
    -- asGodotFormat shmFmt = case shmFmt of
    --   WlShmFormatArgb8888 -> Image.FORMAT_RGBA8
    --   _ -> error $ "Unknown SHM format " ++ show shmFmt
