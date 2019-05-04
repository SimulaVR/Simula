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

import           Data.Typeable
import           Godot.Gdnative.Types
import           Godot.Nativescript

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

-- Wish there was a more elegant way to jam values into these fields at classInit
data GodotSimulaViewSprite = GodotSimulaViewSprite
  { _gsvsObj            :: GodotObject
  , _gsvsServer         :: TVar GodotSimulaServer    -- Contains the WlrSeat
  , _gsvsShouldMove     :: TVar Bool
  , _gsvsSprite         :: TVar GodotSprite3D
  , _gsvsShape          :: TVar GodotBoxShape
  , _gsvsView           :: TVar SimulaView -- Contains Wlr data
  , _gsvsViewport       :: TVar GodotViewport
  -- , gsvsGeometry     :: GodotRect2
  -- , gsvsWlrSeat      :: GodotWlrSeat
  -- , gsvsInputMode    :: TVar InteractiveMode
  }

data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer -- Can obtain WlrSeat
  -- , _svWlrSurface              :: GodotWlrSurface -- Can obtain GodotWlrSurface from GodotWlrXdgSurface
  , _svMapped                  :: TVar Bool
  , _svWlrXdgSurface         :: GodotWlrXdgSurface -- Contains the WlrSurface, its texture data, & even its subsurfaces (via `surface_at`).
  , _gsvsUUID                  :: Maybe UUID
  }

instance Eq SimulaView where
  (==) sv1 sv2 = (_gsvsUUID sv1) == (_gsvsUUID sv2)

-- Required for M.lookup calls on (M.Map SimulaView GodotSimulaViewSprite)
instance Ord SimulaView where
  (<=) sv1 sv2 = (_gsvsUUID sv1) <= (_gsvsUUID sv2)

-- At some point it might be useful to let each sprite have state keeping track of whether it's being moved, resized, etc.
-- data InteractiveMode
--   = InteractivePassthrough -- i.e., VR controllers can just point at it as normally
--   | InteractiveMove
--   | InteractiveResize

makeLenses ''GodotSimulaViewSprite
makeLenses ''SimulaView
makeLenses ''GodotSimulaServer

data SurfaceLocalCoordinates    = SurfaceLocalCoordinates (Float, Float)
data SubSurfaceLocalCoordinates = SubSurfaceLocalCoordinates (Float, Float)

-- Godot helper functions (should eventually be exported to godot-extra).

connectGodotSignal :: (GodotObject :< source) -- , GodotObject :< method_object)
                   => (GodotObject :< method_object)
                   => source                  -- signal source
                   -> String                  -- signal name
                   -> method_object           -- Godot object which has the method being attached to the signal
                   -> String                  -- Name of method to attach to signal
                   -> [GodotVariant]       -- default arguments to supply to method (jammed /after/ manual arguments supplied)
                   -> IO (Int)                --
connectGodotSignal sourceObj signalName methodObj methodName defaultArgs = do
  putStrLn "connectGodotSignal"
  let sourceObj' = safeCast sourceObj      :: GodotObject
  signalName'    <- (toLowLevel (pack signalName))  :: IO GodotString
  let methodObj' =  safeCast methodObj     :: GodotObject
  methodName'    <- (toLowLevel (pack methodName))  :: IO GodotString
  defaultArgs'   <- (toLowLevel defaultArgs) :: IO GodotArray -- Wraps godot_array_new; do we have to clean this up via godot_array_destroy ?
  G.connect sourceObj' signalName' methodObj' methodName' defaultArgs' 0

addChild :: (GodotNode :< parent)
         => (GodotObject :< child)
         => parent
         -> child
         -> IO ()
addChild parent child = do
  G.add_child ((safeCast parent) :: GodotNode )
              ((safeCast child) :: GodotObject)
              True -- Sets legible_unique_name flag to True


removeChild :: (GodotNode :< parent)
               => (GodotObject :< child)
               => parent
               -> child
               -> IO ()
removeChild parent child = do
  G.remove_child ((safeCast parent) :: GodotNode )
                 ((safeCast child) :: GodotObject)

asGodotVariant :: (GodotObject :< godot_type) => godot_type -> IO (GodotVariant)
asGodotVariant godotComplexObj = do
  -- Original method:
  -- godotVariant <- toLowLevel (VariantObject (safeCast godotComplexObj)) :: IO GodotVariant

  -- More idiomatic method:
  godotVariant2 <- (toLowLevel (toVariant ((safeCast godotComplexObj) :: GodotObject))) :: IO GodotVariant

  return godotVariant2

-- | These helper functions can be used to jam Haskell types into emit_signal, and
-- | then retrieve them in their corresponding signal handlers. Use these
-- | functions *only* for (i) registered types that are (ii) instantiated with
-- | classInit. If you try to use these functions with a registered type that
-- | isn't instantiated with classInit, then it could break your program at run-time.
regToVariant :: (GodotObject :< object) => (GodotClass object) => object -> IO (Variant 'GodotTy)
regToVariant obj = return $ VariantObject (safeCast obj) :: IO (Variant 'GodotTy)

variantToReg :: (GodotClass a, Typeable a) => GodotVariant -> IO (Maybe a)
variantToReg godotVariant = do
  -- How it's done in Simula.hs:
  -- godotVariantObj <- fromGodotVariant godotVariant :: IO GodotObject
  -- Just a <- tryObjectCast godotVariantObj
  -- return a

  godotVariant' <- fromLowLevel godotVariant
  ret <- case godotVariant' of
              (VariantObject registeredTypeAsObj) -> tryObjectCast registeredTypeAsObj -- tryObjectCast should return Nothing when this object isn't registered
              _ -> return Nothing
  return ret

-- | Helper function to emit signals with registered arguments that inherit from
-- | GodotObject (like, i.e., GodotSimulaViewSprite).
emitSignal :: (GodotObject :< a)
           => (GodotClass b, Typeable b, GodotObject :< b)
           => a      -- Object emitting the signal
           -> String -- Signal name
           -> [b]    -- Arguments emitted (must be registered types that inherit from GodotObject like i.e. GodotSimulaViewSprite)
           -> IO ()
emitSignal signalEmitter signalName signalArgs = do
  putStrLn "emitSignal"
  let signalEmitter' = (safeCast signalEmitter) :: GodotObject
  signalName'        <- toLowLevel (pack signalName) :: IO GodotString
  signalArgs'        <- mapM regToVariant signalArgs
  -- G.emit_signal :: GodotObject        -- Object emitting the signal
  --               -> GodotString        -- Signal name
  --               -> [Variant 'GodotTy] -- Signal argumetns
  --               -> IO (GodotVariant)  -- Some sort of exit code?
  G.emit_signal signalEmitter' signalName' signalArgs'
  return ()


-- | This is equivalent to the old version of newNS' (formerly called unsafeNewNS)
-- | from godot-extra. The way we were using the new version was causing type
-- | casting errors, so we revert to the old one for now.
newNS'' :: (GodotObject :< a)
  => (GodotObject -> a) -> Text -> [Variant 'GodotTy] -> Text -> IO a
newNS'' constr clsName args url = do
  newNSOld constr clsName args url >>= \case
    Just ns -> return ns
    Nothing -> error $ "Error" -- "Could not instance the " ++ (unpack clsName) ++ " from " ++ (unpack url)
  where
    newNSOld constr clsName args url = do
      load GodotNativeScript "NativeScript" url >>= \case
        Just ns -> (G.new (ns :: GodotNativeScript) args :: IO GodotObject)
          >>= asClass constr clsName
        Nothing -> return Nothing