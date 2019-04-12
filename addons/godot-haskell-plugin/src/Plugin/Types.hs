{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Plugin.Types where

import           Control.Monad
import           Data.Coerce

import           Plugin.Imports
import           Godot.Extra.Register

import qualified Godot.Methods               as G
import           Godot.Gdnative.Types -- for Variant access
import           Godot.Gdnative.Internal.Api as Api

import qualified Godot.Core.GodotImage       as Image
import           Control.Lens                hiding (Context)

import           Foreign
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           System.IO.Unsafe

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

import Data.UUID

C.initializeSimulaCtxAndIncludes

-- We use TVar excessively since these datatypes must be retrieved from the
-- scene graph (requiring IO)
data GodotSimulaServer = GodotSimulaServer 
  { _gssObj                  :: GodotObject
  , _gssWaylandDisplay       :: TVar GodotWaylandDisplay
  , _gssWlrBackend            :: TVar GodotWlrBackend
  , _gssWlrOutput            :: TVar GodotWlrOutput
  , _gssWlrCompositor        :: TVar GodotWlrCompositor
  , _gssWlrXdgShell          :: TVar GodotWlrXdgShell
  , _gssWlrSeat              :: TVar GodotWlrSeat -- Probably make this a TVar since you have to find the node for this in the scene graph.
  , _gssWlrDataDeviceManager :: TVar GodotWlrDataDeviceManager
  , _gssWlrKeyboard          :: TVar GodotWlrKeyboard -- "
  , _gssViews                :: TVar (M.Map SimulaView GodotSimulaViewSprite)
  }

data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer -- Can obtain WlrSeat
  -- , _svWlrSurface              :: GodotWlrSurface -- Can obtain GodotWlrSurface from GodotWlrXdgSurface
  , _svMapped                  :: TVar Bool
  , _gsvsWlrXdgSurface         :: GodotWlrXdgSurface -- Contains the WlrSurface, its texture data, & even its subsurfaces (via `surface_at`).
  , _gsvsUUID                  :: Maybe UUID
  }

instance Eq SimulaView where
  (==) sv1 sv2 = (_gsvsUUID sv1) == (_gsvsUUID sv2)

-- Required for M.lookup calls on (M.Map SimulaView GodotSimulaViewSprite)
instance Ord SimulaView where
  (<=) sv1 sv2 = (_gsvsUUID sv1) <= (_gsvsUUID sv2)

-- Wish there was a more elegant way to jam values into these fields at classInit
data GodotSimulaViewSprite = GodotSimulaViewSprite
  { _gsvsObj            :: GodotObject
  , _gsvsServer         :: TVar GodotSimulaServer    -- Contains the WlrSeat
  , _gsvsShouldMove     :: TVar Bool
  , _gsvsSprite         :: TVar GodotSprite3D
  , _gsvsShape          :: TVar GodotBoxShape
  , _gsvsView           :: TVar SimulaView -- Contains Wlr data
  -- , gsvsGeometry     :: GodotRect2
  -- , gsvsWlrSeat      :: GodotWlrSeat
  -- , gsvsInputMode    :: TVar InteractiveMode
  }

-- At some point it might be useful to let each sprite have state keeping track of whether it's being moved, resized, etc.
-- data InteractiveMode
--   = InteractivePassthrough -- i.e., VR controllers can just point at it as normally
--   | InteractiveMove
--   | InteractiveResize

makeLenses ''GodotSimulaViewSprite
makeLenses ''SimulaView
makeLenses ''GodotSimulaServer

connectGodotSignal :: (GodotObject :< source) -- , GodotObject :< method_object)
                   => (GodotObject :< method_object)
                   => source                  -- signal source
                   -> String                  -- signal name
                   -> method_object           -- Godot object which has the method being attached to the signal
                   -> String                  -- Name of method to attach to signal
                   -> [GodotVariant]       -- default arguments to supply to method (jammed /after/ manual arguments supplied)
                   -> IO (Int)                -- 
connectGodotSignal sourceObj signalName methodObj methodName defaultArgs = do
  let sourceObj' = safeCast sourceObj      :: GodotObject
  signalName'    <- (toLowLevel (pack signalName))  :: IO GodotString
  let methodObj' =  safeCast methodObj     :: GodotObject
  methodName'    <- (toLowLevel (pack methodName))  :: IO GodotString
  defaultArgs'   <- (toLowLevel defaultArgs) :: IO GodotArray -- Wraps godot_array_new; do we have to clean this up via godot_array_destroy ?
  G.connect sourceObj' signalName' methodObj' methodName' defaultArgs' 0
  return 1