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
import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
-- import           Graphics.Wayland.WlRoots.Compositor
import           Graphics.Wayland.WlRoots.Output
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.WlRoots.Backend
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.Render
-- import           Graphics.Wayland.WlRoots.Render.Color
-- import           Graphics.Wayland.WlRoots.OutputLayout
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Seat
-- import           Graphics.Wayland.WlRoots.Cursor
-- import           Graphics.Wayland.WlRoots.XCursorManager
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Input.Keyboard
-- import           Graphics.Wayland.WlRoots.Input.Pointer
-- import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.Input.Buttons
-- import           Graphics.Wayland.WlRoots.Box
import qualified Data.Map.Strict as M

C.initializeSimulaCtxAndIncludes

-- Placing these types (some dummy) here for now
data GodotSimulaServer = GodotSimulaServer
  { _gssObj          :: GodotObject
  , _gssDisplay      :: DisplayServer -- add this.
  , _gssViews        :: TVar (M.Map SimulaView GodotSimulaViewSprite)
  , _gssBackend       :: Ptr Backend
  , _gssXdgShell      :: Ptr WlrXdgShell
  , _gssSeat          :: Ptr WlrSeat
  , _gssKeyboards     :: TVar [SimulaKeyboard]

  -- I think we might need a dummy output global to trick clients into rendering surfaces
  -- If this turns out to not be true, then delete this:
  , _gssOutputs       :: TVar [SimulaOutput] 
  , _gssRenderer      :: Ptr Renderer -- Same story: might need a dummy renderer for certain initialization calls

  -- All ListenerToken's should be manually destroyed when this type is destroyed
  , _gssNewXdgSurface :: ListenerToken
  -- , _gssNewInput      :: ListenerToken -- Not needed now that we're using wlr_godot_backend
  -- , _gssNewOutput     :: ListenerToken -- "

  -- The following datatypes will likely be used/modified for Simula's resizing/movement operations
    -- , _ssCursorMode           :: TVar SimulaCursorMode
    -- , _ssGrabbedView          :: TVar (Maybe SimulaView)
    -- , _ssGrab                 :: TVar (Maybe SurfaceLocalCoordinates)
    -- , _ssResizeEdges          :: TVar (Maybe Int) -- New datatype pending on resizing task

  -- The following probably aren't needed, or need to be wrapped up in a different datatype:
    -- , _ssGrabWidth            :: TVar (Maybe Int) -- Refers to original width of window being resized
    -- , _ssGrabHeight           :: TVar (Maybe Int) -- Refers to origianl height of window being resized

  }
data SimulaOutput = SimulaOutput { _soServer         :: GodotSimulaServer
                                 , _soWlrOutput      :: Ptr WlrOutput
                                 }

data SimulaKeyboard = SimulaKeyboard
  { _skServer    :: GodotSimulaServer
  , _skDevice    :: Ptr InputDevice
  , _skModifiers :: ListenerToken -- TODO: Destroy this somewhere (keyboard destroyer listener)
  , _skKey       :: ListenerToken -- "
  }


-- Temporary home for needed helper types/functions from SimulaServer.hs
data SurfaceLocalCoordinates = SurfaceLocalCoordinates (Double, Double)
data SubSurfaceLocalCoordinates = SubSurfaceLocalCoordinates (Double, Double)
data SurfaceDimension = SurfaceDimension (Int, Int)

data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer
  , _svXdgSurface              :: Ptr WlrXdgSurface
  , _svMapped                  :: TVar Bool
  , _svMap                     :: ListenerToken
  , _svUnmap                   :: ListenerToken
  , _svDestroy                 :: ListenerToken
  , _svCommit                  :: ListenerToken
--, _svRequestMove             :: ListenerToken -- Pending move implementation
--, _svRequestResize           :: ListenerToken -- Pending resize implementation
  }

-- | We will say that two views are "equal" when they have the same Ptr WlrXdgSurface (this is
-- | is a bad idea long-term but for now will suffice; for example, we might have two
-- | distinct views [one used as an icon and one a window] that have the same underlying surface).
-- | TODO: Give SimulaView's a unique id and change this function accordingly.
instance Eq SimulaView where
  (==) = (==) `on` _svXdgSurface

-- Required for M.lookup calls on (M.Map SimulaView GodotSimulaViewSprite)
instance Ord SimulaView where
  (<=) = (<=) `on` _svXdgSurface

-- type ViewMap = M.Map SimulaView GodotSimulaViewSprite
data GodotSimulaViewSprite = GodotSimulaViewSprite
  { _gsvsObj        :: GodotObject
  , _gsvsShouldMove :: TVar Bool
  , _gsvsSprite     :: TVar GodotSprite3D
  , _gsvsShape      :: TVar GodotBoxShape
  , _gsvsTexture    :: TVar GodotSimulaViewTexture
  -- , _gsvsSeat    :: TVar WestonSeat -- Accessible from GodotSimulaViewTexture argument
  }

data GodotSimulaViewTexture = GodotSimulaViewTexture
  { _gsvtObj       :: GodotObject
  , _gsvtView      :: TVar SimulaView
  , _gsvtImage     :: TVar GodotImage
  , _gsvtImageData :: TVar GodotPoolByteArray
  }

makeLenses ''GodotSimulaViewSprite
makeLenses ''GodotSimulaViewTexture
makeLenses ''SimulaView
makeLenses ''GodotSimulaServer

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
  classSignals = []

instance HasBaseClass GodotSimulaViewTexture where
  type BaseClass GodotSimulaViewTexture = GodotImageTexture
  super (GodotSimulaViewTexture obj _ _ _) = GodotImageTexture obj

newGodotSimulaViewTexture :: SimulaView -> IO GodotSimulaViewTexture
newGodotSimulaViewTexture simulaView = do
  ret <- newNS' [] "res://addons/godot-haskell-plugin/SimulaViewTexture.gdns"
  objPtr <- godot_nativescript_get_userdata ret
  texture <- deRefStablePtr $ castPtrToStablePtr objPtr

  -- New:
  atomically $ writeTVar (texture ^. gsvtView) simulaView
  updateSimulaViewTexture texture

  return texture

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
