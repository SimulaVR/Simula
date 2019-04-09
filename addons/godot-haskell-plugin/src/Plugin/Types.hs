{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Types where

import           Data.Coerce
import           Control.Monad
import           Data.Coerce

import           Plugin.Imports
import           Godot.Extra.Register

import qualified Godot.Methods               as G
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
import           Data.Coerce

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

-- We use TVar excessively since these datatypes must be retrieved from the
-- scene graph (requiring IO)
data GodotSimulaServer = GodotSimulaServer 
  { _gssObj                  :: GodotObject
  , _gssWaylandDisplay       :: TVar GodotWaylandDisplay
  , _gsWlrBackend            :: TVar GodotWlrBackend
  , _gssWlrOutput            :: TVar GodotWlrOutput
  , _gssWlrCompositor        :: TVar GodotWlrCompositor
  , _gssWlrXdgShell          :: TVar GodotWlrXdgShell
  , _gssWlrSeat              :: TVar GodotWlrSeat -- Probably make this a TVar since you have to find the node for this in the scene graph.
  , _gssWlrDataDeviceManager :: TVar GodotWlrDataDeviceManager
  , _gssWlrKeyboard          :: TVar GodotWlrKeyboard -- "
  , _gssViews                :: TVar (M.Map SimulaView GodotSimulaViewSprite)
  }

data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer
  -- , _svWlrSurface              :: GodotWlrSurface -- Might be unneeded
  , _svMapped                  :: TVar Bool
  , _gsvsWlrXdgSurface         :: GodotWlrXdgSurface -- Contains the WlrSurface, its texture data, & even its subsurfaces (via `surface_at`).
  }

-- | We will say that two views are "equal" when they have the same Ptr WlrXdgSurface (this is
-- | is a bad idea long-term but for now will suffice; for example, we might have two
-- | distinct views [one used as an icon and one a window] that have the same underlying surface).
-- | TODO: Give SimulaView's a unique id and change this function accordingly.
instance Eq SimulaView where
  -- (==) = (==) `on` _svWlrSurface
  -- (==) sv1 sv2 = G.get_wlr_surface (_gsvsWlrXdgSurface sv1) == G.get_wlr_surface (_gsvsWlrXdgSurface sv2)
  (==) x y = True

-- Required for M.lookup calls on (M.Map SimulaView GodotSimulaViewSprite)
instance Ord SimulaView where
  -- (<=) = (<=) `on` _svXdgSurface
  (<=) x y = True

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

{-
-- Need to implement:
instance AsVariant (Ptr a) of
  -- ...

-- | Should already be auto-generated by godot-haskell (gdwlroots branch)
-- get_wlr_surface :: Method "get_wlr_surface" cls sig => cls -> sig
-- get_wlr_surface = runMethod @"get_wlr_surface"

bindWlrSurface_get_wlr_surface
  = unsafePerformIO $
      withCString "WlrSurface" $
        \ clsNamePtr ->
          withCString "get_wlr_surface" $
            \ methodNamePtr ->
              Api.godot_method_bind_get_method clsNamePtr methodNamePtr

-- TODO: Why conceptually do we need this?
{-# NOINLINE bindWlrSurface_get_wlr_surface #-}

-- Opaque type defined in Debug.C:
-- data C'WlrSurface

instance Method "get_wlr_surface" GodotWlrSurface (IO (Ptr C'WlrSurface)) where
        runMethod cls
          = withVariantArray []
              (\ (arrPtr, len) ->
                 Api.godot_method_bind_call bindWlrSurface_get_wlr_surface (coerce cls)
                   arrPtr
                   len
                   >>= \ (err, res) -> throwIfErr err >> fromGodotVariant res)
-}