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

-- data GodotSimulaServer
data GodotWlrSurface

data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer
  , _svWlrSurface              :: GodotWlrSurface
  , _svMapped                  :: TVar Bool
  }

-- | We will say that two views are "equal" when they have the same Ptr WlrXdgSurface (this is
-- | is a bad idea long-term but for now will suffice; for example, we might have two
-- | distinct views [one used as an icon and one a window] that have the same underlying surface).
-- | TODO: Give SimulaView's a unique id and change this function accordingly.
instance Eq SimulaView where
  -- (==) = (==) `on` _svWlrSurface
  (==) x y = True

-- Required for M.lookup calls on (M.Map SimulaView GodotSimulaViewSprite)
instance Ord SimulaView where
  -- (<=) = (<=) `on` _svXdgSurface
  (<=) x y = True

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

