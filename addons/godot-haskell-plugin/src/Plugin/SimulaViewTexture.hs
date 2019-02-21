{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.SimulaViewTexture where

import           Data.Coerce
import           Control.Monad
import           Data.Coerce

import           Plugin.Imports
import           Godot.Extra.Register

import qualified Godot.Methods               as G
import           Godot.Gdnative.Internal.Api

import qualified Godot.Core.GodotImage       as Image
import           Control.Lens                hiding (Context)

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Buffer
import           Graphics.Wayland.Internal.SpliceServerTypes (Buffer(..))

C.initializeSimulaCtxAndIncludes

-- Placing these types (some dummy) here for now
data GodotSimulaServer
data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer
  , _svXdgSurface              :: Ptr WlrXdgSurface
  , _svMapped                  :: TVar Bool
  , _svMap                     :: ListenerToken
  , _svUnmap                   :: ListenerToken
  , _svDestroy                 :: ListenerToken
--, _svRequestMove             :: ListenerToken -- Pending move implementation
--, _svRequestResize           :: ListenerToken -- Pending resize implementation
  }

data GodotSimulaViewTexture = GodotSimulaViewTexture
  { _gsvtObj       :: GodotObject
  , _gsvtView      :: TVar SimulaView
  , _gsvtImage     :: TVar GodotImage
  , _gsvtImageData :: TVar GodotPoolByteArray
  }

makeLenses ''GodotSimulaViewTexture
makeLenses ''SimulaView

instance GodotClass GodotSimulaViewTexture where
  godotClassName = "SimulaSurfaceTexture"

instance ClassExport GodotSimulaViewTexture where
  classInit obj = do
    img <- unsafeInstance GodotImage "Image"
    imgdt <- godot_pool_byte_array_new
    GodotSimulaViewTexture obj
      <$> atomically (newTVar (error "SimulaViewTexture not initialized."))
      <*> atomically (newTVar img)
      <*> atomically (newTVar imgdt)
  classExtends = "ImageTexture"
  classMethods = []

instance HasBaseClass GodotSimulaViewTexture where
  type BaseClass GodotSimulaViewTexture = GodotImageTexture
  super (GodotSimulaViewTexture obj _ _ _) = GodotImageTexture obj

newGodotSimulaViewTexture :: IO GodotSimulaViewTexture
newGodotSimulaViewTexture = do
  ret <- unsafeNewNS id "Object" [] "res://addons/godot-haskell-plugin/SimulaViewTexture.gdns"
  objPtr <- godot_nativescript_get_userdata ret
  deRefStablePtr $ castPtrToStablePtr objPtr

-- | This function inefficiently copies the entire buffer to Godot.
-- | TODO: Make this more efficient.
updateSimulaViewTexture :: GodotSimulaViewTexture -> IO ()
updateSimulaViewTexture gsvt = do
  simulaView <- atomically $ readTVar (gsvt ^. gsvtView)
  maybeSurface <- xdgSurfaceGetSurface (simulaView ^. svXdgSurface)
  case maybeSurface of
    Nothing -> return ()
    Just surface -> do
      hasBuffer <- surfaceHasBuffer surface
      buffer <- surfaceGetBuffer surface
      maybeRes <- getBufferResource buffer
      case maybeRes of
        Nothing -> return ()
        Just res -> do
          maybeShmbuf <- shmBufferGet (coerce res) -- Am unsure if this coerce will work; if debugging this function check this.
          case maybeShmbuf of
            Nothing -> return ()
            Just shmbuf -> do
              return ()
              dt <- shmBufferGetData shmbuf
              width <- shmBufferGetWidth shmbuf
              height <- shmBufferGetHeight shmbuf
              stride <- shmBufferGetStride shmbuf
              shmFmt <- shmBufferGetFormat shmbuf
              fmt <- asGodotFormat shmFmt
              let size :: Integral a => a
                  size = fromIntegral (stride * height)
              img <- atomically $ readTVar (_gsvtImage gsvt)
              byteArray <- godot_pool_byte_array_new
              cursize <- godot_pool_byte_array_size byteArray
              when (cursize /= size) $
                godot_pool_byte_array_resize byteArray size
              writeAccess <- godot_pool_byte_array_write byteArray
              writePtr <- godot_pool_byte_array_write_access_ptr writeAccess
              allocaArray size $ \arrPtr -> do
                shmBufferBeginAccess shmbuf
                copyBytes arrPtr (castPtr dt) size
                shmBufferEndAccess shmbuf
                let arrPtr32 = castPtr arrPtr :: Ptr Word32
                forM_ [0 .. size `div` 4] $ \n -> do
                  let ptr = advancePtr arrPtr32 n
                  elem <- peek ptr
                                  -- HACKHACK
                  let [b3, b2, b1, b0] =
                        map (\n -> shiftR elem (n * 8) .&. 0xff) [0 .. 3]
                  let result =
                        foldr (\n res -> shiftL res 8 .|. n) 0 [b1, b2, b3, b0]
                  poke ptr result
                copyBytes writePtr arrPtr size
              godot_pool_byte_array_write_access_destroy writeAccess -- TODO helper function
              G.create_from_data img width height False fmt byteArray
              G.create_from_image gsvt img (7 .|. 16 :: Int)
              godot_pool_byte_array_destroy byteArray
              return ()
              where asGodotFormat shmFmt = do
                      format <- fromIntegral <$> [C.exp| int wl_shm_format { WL_SHM_FORMAT_ARGB8888 } |]
                      return $ case shmFmt of
                                 format -> Image.FORMAT_RGBA8
                                 _ -> error $ "Unknown SHM format " ++ show shmFmt
