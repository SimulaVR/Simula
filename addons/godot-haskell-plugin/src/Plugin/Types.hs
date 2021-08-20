{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Plugin.Types where

import           System.Directory
import           Linear.Matrix
import           Linear.V3

import           Data.Time.Clock
import           Data.Maybe
import           Control.Concurrent
import           Control.Monad
import           Data.Coerce
import           Unsafe.Coerce
import           Control.Exception.Safe

import           Data.Typeable
import           Godot.Gdnative.Types
import           Godot.Nativescript

import           Data.Colour
import           Data.Colour.SRGB.Linear

import           Plugin.Imports

import           Godot.Core.GodotVisualServer          as G
import qualified Godot.Methods               as G
import           Godot.Gdnative.Types -- for Variant access
import           Godot.Gdnative.Internal.Api as Api
import           Godot.Nativescript as NativeScript

import qualified Godot.Core.GodotImage       as G
import           Control.Lens                hiding (Context)

import           Foreign
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Marshal.Alloc

import           Foreign.C.Types
import qualified Language.C.Inline as C

import           System.IO.Unsafe
import           Data.Monoid
import           Data.List
-- import           Data.Text as T
import           Data.Text.Encoding
import           Data.Text
import qualified System.Process.ByteString as B
import qualified Data.ByteString.Char8 as B

import Data.IORef

import qualified Data.Map.Strict as M

import Data.UUID
import System.Process
import System.Process.Internals
import System.Posix.Types
import GHC.IO.Handle

import Godot.Core.GodotViewport as G

import Data.Map.Ordered as MO
import Dhall
import qualified Data.Vector as V
import System.IO.Streams.Process
import System.IO.Streams.Internal
import qualified Data.ByteString as B
import System.IO.Streams.Text

instance Show Transform where
  show tf = (show (_tfBasis tf)) ++ (" w/position: ") ++ (show (_tfPosition tf))

instance Show GodotWlrXWaylandSurface where
  show wlrXWaylandSurface = (show (coerce wlrXWaylandSurface :: Ptr ()))
instance Show GodotWlrXdgSurface where
  show wlrXWaylandSurface = (show (coerce wlrXWaylandSurface :: Ptr ()))
instance Show GodotWlrSurface where
  show wlrSurface = (show (coerce wlrSurface :: Ptr ()))

instance Ord GodotWlrXWaylandSurface where
  wlrXWaylandSurface1 `compare` wlrXWaylandSurface2 = ((coerce wlrXWaylandSurface1) :: Ptr ()) `compare` ((coerce wlrXWaylandSurface2) :: Ptr ())

instance Ord GodotSpatial where
  sp1 `compare` sp2 = ((coerce sp1) :: Ptr ()) `compare` ((coerce sp2) :: Ptr ())

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = f b >>= \case
  Just (a, b') -> return . (a :) =<< unfoldrM f b'
  _            -> return []

data SurfaceLocalCoordinates    = SurfaceLocalCoordinates (Float, Float)
data SubSurfaceLocalCoordinates = SubSurfaceLocalCoordinates (Float, Float)
data SpriteDimensions      = SpriteDimensions (Int, Int)

data ResizeMethod = Zoom | Horizontal | Vertical deriving (Eq)

-- This should ideally be `[Variant 'HaskellTy]`, but that would
-- require `AsVariant` to handle both `LibType`s.
type instance TypeOf 'HaskellTy GodotArray = [GodotVariant]
instance GodotFFI GodotArray [GodotVariant] where
  fromLowLevel vs = do
    size <- fromIntegral <$> Api.godot_array_size vs
    forM [0..size-1] $ Api.godot_array_get vs

  toLowLevel vs = do
    array <- Api.godot_array_new
    mapM_ (Api.godot_array_append array) vs
    return array

data KeyboardShortcut = KeyboardShortcut {
  _keyCombination :: [String]
, _keyAction      :: String
} deriving (Generic, Show)

data KeyboardRemapping = KeyboardRemapping {
  _keyOriginal :: String
, _keyMappedTo :: String
} deriving (Generic, Show)

data StartingApps = StartingApps {
  _center :: Maybe String
, _right  :: Maybe String
, _bottom :: Maybe String
, _left   :: Maybe String
, _top    :: Maybe String
} deriving (Generic, Show)

data Configuration = Configuration {
  _backend :: String
, _startingApps :: StartingApps
, _defaultWindowResolution :: Maybe (Natural, Natural)
, _defaultWindowScale :: Double
, _axisScrollSpeed :: Double
, _mouseSensitivityScaler :: Double
, _keyBindings :: [KeyboardShortcut]
, _keyRemappings :: [KeyboardRemapping]
, _environmentsDirectory :: String
, _environmentDefault :: String
-- , _defaultTransparency :: Double -- Remove until order independent transparency is implemented
, _scenes :: [String]
, _hudConfig :: String
} deriving (Generic, Show)

instance FromDhall KeyboardRemapping
instance FromDhall KeyboardShortcut
instance FromDhall Configuration
instance FromDhall StartingApps

type Scancode          = Int
type Modifiers         = Int
type Keycode           = Int
type SpriteLocation    = Maybe (GodotSimulaViewSprite, SurfaceLocalCoordinates)
type KeyboardAction    = SpriteLocation -> Bool -> IO () -- `Bool` signifies whether the key is pressed down
type KeyboardShortcuts = M.Map (Modifiers, Keycode) KeyboardAction
type KeyboardRemappings = M.Map Scancode Scancode
type StartingAppsLaunched = Int
type StartingAppsRemaining = Int

data SimulaEnvironment = Day | Night
  deriving (Eq, Show)

data Grab = GrabWindow GodotSimulaViewSprite Float | GrabWindows GodotTransform | GrabWorkspaces GodotTransform
type DiffMap = M.Map GodotSpatial GodotTransform

data HUD = HUD
  { _hudCanvasLayer :: GodotCanvasLayer
  , _hudRtlWorkspace :: GodotRichTextLabel
  , _hudRtlI3        :: GodotRichTextLabel
  , _hudSvrTexture   :: GodotTexture
  , _hudDynamicFont  :: GodotDynamicFont
  , _hudI3Status     :: String
  }

-- We use TVar excessively since these datatypes must be retrieved from the
-- scene graph (requiring IO)
data GodotSimulaServer = GodotSimulaServer
  { _gssObj                   :: GodotObject
  , _gssWaylandDisplay        :: TVar GodotWaylandDisplay
  , _gssWlrBackend            :: TVar GodotWlrBackend
  , _gssWlrOutput             :: TVar GodotWlrOutput
  , _gssWlrCompositor         :: TVar GodotWlrCompositor
  , _gssWlrXdgShell           :: TVar GodotWlrXdgShell
  , _gssWlrXWayland           :: TVar GodotWlrXWayland
  , _gssWlrSeat               :: TVar GodotWlrSeat -- Probably make this a TVar since you have to find the node for this in the scene graph.
  , _gssWlrDataDeviceManager  :: TVar GodotWlrDataDeviceManager
  , _gssWlrKeyboard           :: TVar GodotWlrKeyboard -- "
  , _gssViews                 :: TVar (M.Map SimulaView GodotSimulaViewSprite)
  , _gssKeyboardFocusedSprite :: TVar (Maybe GodotSimulaViewSprite) -- <- Here
  , _gssVisualServer          :: TVar GodotVisualServer
  , _gssActiveCursorGSVS      :: TVar (Maybe GodotSimulaViewSprite)
  , _gssCursorTexture         :: TVar (Maybe GodotTexture)
  , _gssScreenshotCursorTexture  :: TVar (Maybe GodotTexture)
  , _gssHMDRayCast            :: TVar (GodotRayCast)
  , _gssKeyboardGrabbedSprite :: TVar (Maybe (GodotSimulaViewSprite, Float)) -- We encode both the gsvs and its original distance from the user
  , _gssXWaylandDisplay       :: TVar (Maybe String) -- For appLaunch
  , _gssOriginalEnv           :: [(String, String)]
  , _gssFreeChildren          :: TVar (M.Map GodotWlrXWaylandSurface GodotSimulaViewSprite)
  , _gssConfiguration         :: TVar Configuration
  , _gssKeyboardShortcuts     :: TVar KeyboardShortcuts
  , _gssKeyboardRemappings    :: TVar KeyboardRemappings
  , _gssAxisScrollSpeed       :: TVar Double
  , _gssMouseSensitivityScaler :: TVar Double
  , _gssStartingApps          :: TVar [String]
  , _gssWorldEnvironment      :: TVar (GodotWorldEnvironment, String)
  , _gssEnvironmentTextures   :: TVar [String]
  , _gssStartingAppTransform  :: TVar (Maybe GodotTransform)
  , _gssPid                   :: String
  , _gssStartingAppPids       :: TVar (M.Map ProcessID [String])
  , _gssGrab                  :: TVar (Maybe Grab)
  , _gssDiffMap               :: TVar (M.Map GodotSpatial GodotTransform)
  , _gssWorkspaces            :: Vector GodotSpatial
  , _gssWorkspace             :: TVar (GodotSpatial, String)
  , _gssWorkspacePersistent   :: TVar GodotSpatial
  , _gssHUD                   :: TVar HUD
  , _gssScenes                :: TVar [String]
  , _gssScene                 :: TVar (Maybe (String, GodotNode))
  , _gssWasdInitialRotation   :: TVar Float
  , _gssWasdMode              :: TVar Bool
  , _gssCanvasAR              :: TVar CanvasAR
  , _gssScreenRecorder        :: TVar (Maybe ProcessHandle)
  }

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  = GodotSpatial obj

type SurfaceMap = OMap GodotWlrSurface CanvasSurface

data GodotSimulaViewSprite = GodotSimulaViewSprite
  { _gsvsObj               :: GodotObject
  , _gsvsServer            :: TVar GodotSimulaServer
  , _gsvsShouldMove        :: TVar Bool
  , _gsvsMeshInstance      :: TVar GodotMeshInstance
  , _gsvsShape             :: TVar GodotBoxShape
  , _gsvsView              :: TVar SimulaView
  , _gsvsCanvasBase        :: TVar CanvasBase
  , _gsvsCanvasSurface     :: TVar CanvasSurface
  , _gsvsCursorCoordinates :: TVar SurfaceLocalCoordinates
  , _gsvsTargetSize        :: TVar (Maybe SpriteDimensions)
  , _gsvsFreeChildren      :: TVar [GodotWlrXWaylandSurface]
  , _gsvsTransparency      :: TVar Float
  , _gsvsScreenshotMode    :: TVar Bool
  , _gsvsScreenshotCoords  :: TVar (Maybe SurfaceLocalCoordinates, Maybe SurfaceLocalCoordinates)
  , _gsvsActiveSurface     :: TVar (Maybe GodotWlrSurface)
  , _gsvsFrameCount        :: TVar Integer
  , _gsvsSpilloverDims     :: TVar (Maybe (Int, Int))
  , _gsvsResizedLastFrame  :: TVar Bool
  , _gsvsCursor            :: TVar ((Maybe GodotWlrSurface), (Maybe GodotTexture))
  , _gsvsIsAtTargetDims    :: TVar Bool
  , _gsvsDamagedRegions    :: TVar [GodotRect2]
  , _gsvsIsDamaged         :: TVar Bool
  }

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  = GodotRigidBody obj

data CanvasBase = CanvasBase {
    _cbObject       :: GodotObject
  , _cbGSVS         :: TVar GodotSimulaViewSprite
  , _cbViewport     :: TVar GodotViewport
}

instance HasBaseClass CanvasBase where
  type BaseClass CanvasBase = GodotNode2D
  super (CanvasBase obj _ _ ) = GodotNode2D obj

data CanvasSurface = CanvasSurface {
    _csObject       :: GodotObject
  , _csGSVS         :: TVar GodotSimulaViewSprite
  , _csViewport     :: TVar GodotViewport
  , _csClearShader  :: TVar GodotShaderMaterial
  , _csPremulShader :: TVar GodotShaderMaterial
  , _csFrameCounter :: TVar Integer
}

instance HasBaseClass CanvasSurface where
  type BaseClass CanvasSurface = GodotNode2D
  super (CanvasSurface obj _ _ _ _ _) = GodotNode2D obj

instance Eq CanvasSurface where
  (==) = (==) `on` _csObject

data CanvasAR = CanvasAR {
    _carObject :: GodotObject
  , _carGSS :: TVar GodotSimulaServer
  -- , _carViewport :: TVar GodotViewport
  , _carCanvasLayer :: TVar GodotCanvasLayer
  , _carShader :: TVar GodotShaderMaterial
  , _carCameraTexture :: TVar GodotCameraTexture
}

instance HasBaseClass CanvasAR where
  type BaseClass CanvasAR = GodotNode2D
  super (CanvasAR obj _ _ _ _) = GodotNode2D obj

data SimulaView = SimulaView
  { _svServer                  :: GodotSimulaServer -- Can obtain WlrSeat
  -- , _svWlrSurface              :: GodotWlrSurface -- Can obtain GodotWlrSurface from GodotWlrXdgSurface
  , _svMapped                  :: TVar Bool
  , _svWlrEitherSurface        :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface -- Contains the WlrSurface, its texture data, & even its subsurfaces (via `surface_at`).
  , _gsvsUUID                  :: Maybe UUID
  }

data GodotPancakeCamera = GodotPancakeCamera {
    _gpcObject     :: GodotObject
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
makeLenses ''CanvasBase
makeLenses ''CanvasSurface
makeLenses ''CanvasAR
makeLenses ''SimulaView
makeLenses ''GodotSimulaServer
makeLenses ''GodotPancakeCamera
makeLenses ''KeyboardShortcut
makeLenses ''KeyboardRemapping
makeLenses ''StartingApps
makeLenses ''Configuration
makeLenses ''HUD

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
  -- putStrLn "connectGodotSignal"
  let sourceObj' = safeCast sourceObj      :: GodotObject
  signalName'    <- (toLowLevel (pack signalName))  :: IO GodotString
  let methodObj' =  safeCast methodObj     :: GodotObject
  methodName'    <- (toLowLevel (pack methodName))  :: IO GodotString
  defaultArgs'   <- (toLowLevel defaultArgs) :: IO GodotArray -- Wraps godot_array_new; do we have to clean this up via godot_array_destroy ?
  G.connect sourceObj' signalName' methodObj' methodName' defaultArgs' 0

addChild :: (GodotNode :< parent)
         => (GodotNode :< child)
         => parent
         -> child
         -> IO ()
addChild parent child = do

  -- instance Method "add_child" GodotNode (GodotNode -> Bool -> IO ())
  G.add_child ((safeCast parent) :: GodotNode )
              ((safeCast child) :: GodotNode)
              True -- Sets legible_unique_name flag to True


removeChild :: (GodotNode :< parent)
               => (GodotNode :< child)
               => parent
               -> child
               -> IO ()
removeChild parent child = do
  G.remove_child ((safeCast parent) :: GodotNode )
                 ((safeCast child) :: GodotNode)

--asGodotVariant :: (GodotObject :< godot_type) => godot_type -> IO (GodotVariant)
--asGodotVariant godotComplexObj = do
  -- Original method:
  -- godotVariant <- toLowLevel (VariantObject (safeCast godotComplexObj)) :: IO GodotVariant

  -- More idiomatic method:
--  godotVariant2 <- (toLowLevel (toVariant ((safeCast godotComplexObj) :: GodotObject))) :: IO GodotVariant

--  return godotVariant2

-- | These helper functions can be used to jam Haskell types into emit_signal, and
-- | then retrieve them in their corresponding signal handlers. Use these
-- | functions *only* for (i) registered types that are (ii) instantiated with
-- | classInit. If you try to use these functions with a registered type that
-- | isn't instantiated with classInit, then it could break your program at run-time.
regToVariant :: (GodotObject :< object) => (NativeScript object) => object -> IO (Variant 'GodotTy)
regToVariant obj = return $ VariantObject (safeCast obj) :: IO (Variant 'GodotTy)

variantToReg :: (NativeScript a, Typeable a) => GodotVariant -> IO (Maybe a)
variantToReg godotVariant = do
  -- How it's done in Simula.hs:
  -- godotVariantObj <- fromGodotVariant godotVariant :: IO GodotObject
  -- Just a <- tryObjectCast godotVariantObj
  -- return a

  godotVariant' <- fromLowLevel godotVariant
  ret <- case godotVariant' of
              (VariantObject registeredTypeAsObj) -> asNativeScript registeredTypeAsObj -- tryObjectCast should return Nothing when this object isn't registered
              _ -> return Nothing
  return ret

-- | Helper function to emit signals with registered arguments that inherit from
-- | GodotObject (like, i.e., GodotSimulaViewSprite).
emitSignal :: (GodotObject :< a)
           => (NativeScript b, Typeable b, GodotObject :< b)
           => a      -- Object emitting the signal
           -> String -- Signal name
           -> [b]    -- Arguments emitted (must be registered types that inherit from GodotObject like i.e. GodotSimulaViewSprite)
           -> IO ()
emitSignal signalEmitter signalName signalArgs = do
  -- putStrLn "emitSignal"
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

-- G.load :: Godot_ResourceLoader -> (GodotString -> GodotString -> Bool -> IO GodotResource)
-- G.asClass :: (GodotObject :< a, a :< b) => (GodotObject -> b) -> Text -> a -> IO (Maybe b)
-- G.asClass = do constr (safeCast a)
-- G.new :: GodotNativeScript -> ([Variant 'GodotTy] -> IO GodotObject)
-- G.asClass
-- GodotResource -> GodotNativeScript -> GodotObject -> Godot

deriving instance Eq GodotWlrOutput
deriving instance Eq GodotWlrXdgSurface
deriving instance Eq GodotWlrXWaylandSurface
deriving instance Eq GodotSpatial

-- Unused/untested.
getGSVSFromEitherSurface :: GodotSimulaServer -> Either GodotWlrXdgSurface GodotWlrXWaylandSurface -> IO (Maybe GodotSimulaViewSprite)
getGSVSFromEitherSurface gss eitherSurface = do
  viewMap <- atomically $ readTVar (_gssViews gss) -- (M.Map SimulaView GodotSimulaViewSprite)
  case eitherSurface of
    (Left wlrXdgSurface) -> do
      -- let gsvsList = filter (containsGodotWlrXdgSurface wlrXdgSurface) $ M.keys viewMap
      let filteredMap = M.filterWithKey (containsGodotWlrXdgSurface wlrXdgSurface) viewMap
      let maybeGsvs = Data.Maybe.listToMaybe (M.elems filteredMap)
      return maybeGsvs
    (Right wlrXWaylandSurface) -> do
      -- let gsvsList = filter (containsGodotWlrXWaylandSurface wlrXWaylandSurface) $ M.keys viewMap
      let filteredMap = M.filterWithKey (containsGodotWlrXWaylandSurface wlrXWaylandSurface) viewMap
      let maybeGsvs = Data.Maybe.listToMaybe (M.elems filteredMap)
      return maybeGsvs
  where
    containsGodotWlrXdgSurface :: GodotWlrXdgSurface -> SimulaView -> GodotSimulaViewSprite -> Bool
    containsGodotWlrXdgSurface wlrXdgSurface simulaView _ = 
      ((simulaView ^. svWlrEitherSurface) == Left wlrXdgSurface)
    containsGodotWlrXWaylandSurface :: GodotWlrXWaylandSurface -> SimulaView -> GodotSimulaViewSprite -> Bool
    containsGodotWlrXWaylandSurface wlrXWaylandSurface simulaView _ = 
      ((simulaView ^. svWlrEitherSurface) == Right wlrXWaylandSurface)

{- Import godot-extra functions that godot-haskell still lacks. -}
newNS :: [Variant 'GodotTy] -> Text -> IO (Maybe GodotObject)
newNS args url = do
  load GodotNativeScript "NativeScript" url >>= \case
    Just ns -> Just <$> G.new (ns :: GodotNativeScript) args
    Nothing -> return Nothing

newNS' :: [Variant 'GodotTy] -> Text -> IO GodotObject
newNS' args url = do
  newNS args url >>= \case
    Just ns -> return ns
    Nothing -> error $ "Could not instance class from " ++ (Data.Text.unpack url)

instance' :: (GodotObject :< a) => (GodotObject -> a) -> Text -> IO (Maybe a)
instance' constr className = do
  classDB <- getClassDB
  vt      <- (G.instance' classDB =<< toLowLevel className) >>= fromLowLevel
  case fromVariant vt :: Maybe GodotObject of
    Just obj -> asClass constr className obj
    Nothing  -> return Nothing

unsafeInstance :: (GodotObject :< a) => (GodotObject -> a) -> Text -> IO a
unsafeInstance constr className = instance' constr className >>= \case
  Just a  -> return a
  Nothing -> error $ "Could not instance " ++ (Data.Text.unpack className)

asClass :: (GodotObject :< a, a :< b)
        => (GodotObject -> b)
        -> Text
        -> a
        -> IO (Maybe b)
asClass constr clsName a = do
  isClass' <- a `isClass` clsName
  return $ if isClass' then Just $ constr $ safeCast a else Nothing

asClass' :: (GodotObject :< a, a :< b) => (GodotObject -> b) -> Text -> a -> IO b
asClass' constr clsName a = asClass constr clsName a >>= \case
  Just a' -> return a'
  Nothing -> error $ "Could not cast to " ++ (Data.Text.unpack clsName)

load :: (GodotResource :< a) => (GodotObject -> a) -> Text -> Text -> IO (Maybe a)
load constr clsName url = do
  rl       <- getSingleton Godot_ResourceLoader "ResourceLoader"
  url'     <- toLowLevel url
  clsName' <- toLowLevel clsName
  res      <- G.exists rl url' clsName' >>= \exists ->
    if exists
    then Just <$> G.load rl url' clsName' False
    else return Nothing

  res & \case
    Just a -> asClass constr clsName a
    Nothing -> return Nothing

getClassDB :: IO Godot_ClassDB
getClassDB = Api.godot_global_get_singleton & withCString (unpack "ClassDB")
  >>= asClass' Godot_ClassDB "ClassDB"

-- Leaks
getSingleton :: (GodotObject :< b) => (GodotObject -> b) -> Text -> IO b
getSingleton constr name = do
  engine <- getEngine
  name' <- toLowLevel name
  b <- G.has_singleton engine name'
  res <- if b then G.get_singleton engine name' >>= asClass' constr name
              else error $ "No singleton named " ++ (Data.Text.unpack name)
  Api.godot_string_destroy name'
  return res

isClass :: GodotObject :< a => a -> Text -> IO Bool
isClass obj clsName = do
  objClass' <- G.get_class (GodotNode $ safeCast obj)
  objClass <- fromLowLevel objClass'
  let clsNameSanitized =
        if objClass /= "" && Data.Text.head objClass == '_'
        then "_" `Data.Text.append` clsName
        else clsName
  clsNameSanitized' <- ((toLowLevel clsNameSanitized) :: IO GodotString)
  ret <- G.is_class ((safeCast obj) :: GodotObject) clsNameSanitized'
  Api.godot_string_destroy objClass'
  Api.godot_string_destroy clsNameSanitized'
  return ret

-- Leaks
getEngine :: IO Godot_Engine
getEngine = Api.godot_global_get_singleton & withCString (unpack "Engine")
  >>= asClass' Godot_Engine "Engine"

godotPrint :: Text -> IO ()
godotPrint str = Api.godot_print =<< toLowLevel str

getWlrSurface :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface -> IO GodotWlrSurface
getWlrSurface eitherSurface = do
  case eitherSurface of
    (Left wlrXdgSurface) -> do
      validateSurfaceE wlrXdgSurface
      G.get_wlr_surface wlrXdgSurface
    (Right wlrXWaylandSurface) -> do
      validateSurfaceE wlrXWaylandSurface
      G.get_wlr_surface wlrXWaylandSurface


-- | Used to supply GodotVector2 to
-- |   G.set_size :: GodotViewport -> GodotVector2 -> IO ()
toGodotVector2 :: (Int, Int) -> IO (GodotVector2)
toGodotVector2 (width, height) = do
  let v2 = (V2 (fromIntegral width) (fromIntegral height))
  gv2 <- toLowLevel v2 :: IO (GodotVector2)
  return gv2

-- | Helper to stay memory safe when allocating/destructing gdwlroots types.
withGodot :: (IO godot_alloc)
          -> (godot_alloc -> IO ())
          -> (godot_alloc-> IO a)
          -> IO a
withGodot allocatedType' destr action  = do
  allocatedType <- allocatedType'
  ret <- action allocatedType
  destr allocatedType
  return ret

destroyMaybe :: GodotReference -> IO ()
destroyMaybe ref =
  whenM (G.unreference @GodotReference ref) (Api.godot_object_destroy $ safeCast ref)

getSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotVector3 -> IO (SurfaceLocalCoordinates)
getSurfaceLocalCoordinates gsvs clickPos = do
  lpos <- G.to_local gsvs clickPos >>= fromLowLevel
  meshInstance <- atomically $ readTVar (_gsvsMeshInstance gsvs)
  aabb <- G.get_aabb meshInstance
  quadMesh <- getQuadMesh gsvs
  aabbSize <- godot_aabb_get_size aabb >>= fromLowLevel
  quadMeshSize <- G.get_size quadMesh >>= fromLowLevel

  let topLeftPos =
        V2 (aabbSize ^. _x / 2 - lpos ^. _x) (aabbSize ^. _y / 2 - lpos ^. _y)
  let scaledPos = liftI2 (/) topLeftPos (aabbSize ^. _xy)
  let coords = liftI2 (*) quadMeshSize scaledPos
  -- coords = surface coordinates in pixel with (0,0) at top left
  let sx = fromIntegral $ truncate (1000 * coords ^. _x) -- Adjust by a factor of 1000 since we are dealing with Quad Mesh
      sy = fromIntegral $ truncate (1000 * coords ^. _y) -- "
  clickPos' <- fromLowLevel clickPos
  return (SurfaceLocalCoordinates (sx, sy))

getARVRCameraOrPancakeCameraTransform :: GodotSimulaServer -> IO GodotTransform
getARVRCameraOrPancakeCameraTransform gss = do
  let nodePathStr = "/root/Root/VRViewport/ARVROrigin/ARVRCamera"
  nodePath <- (toLowLevel (pack nodePathStr)) :: IO GodotNodePath
  hasNode  <- G.has_node gss (nodePath :: GodotNodePath)
  transform <- case hasNode of
        False -> do camera <- getViewportCamera gss
                    G.get_camera_transform camera
        True ->  do gssNode  <- G.get_node ((safeCast gss) :: GodotNode) nodePath
                    let arvrCamera = (coerce gssNode) :: GodotARVRCamera -- HACK: We use `coerce` instead of something more proper
                    G.get_transform (arvrCamera)
  Api.godot_node_path_destroy nodePath
  return transform
  where getViewportCamera :: GodotSimulaServer -> IO GodotCamera
        getViewportCamera gss = do
          viewport <- G.get_viewport gss :: IO GodotViewport
          camera <- G.get_camera viewport :: IO GodotCamera
          return camera

getARVRCameraOrPancakeCameraTransformGlobal :: GodotSimulaServer -> IO GodotTransform
getARVRCameraOrPancakeCameraTransformGlobal gss = do
  let nodePathStr = "/root/Root/VRViewport/ARVROrigin/ARVRCamera"
  nodePath <- (toLowLevel (pack nodePathStr)) :: IO GodotNodePath
  hasNode  <- G.has_node gss (nodePath :: GodotNodePath)
  transform <- case hasNode of
        False -> do camera <- getViewportCamera gss
                    G.get_camera_transform camera
        True ->  do gssNode  <- G.get_node ((safeCast gss) :: GodotNode) nodePath
                    let arvrCamera = (coerce gssNode) :: GodotARVRCamera -- HACK: We use `coerce` instead of something more proper
                    G.get_global_transform (arvrCamera)
  Api.godot_node_path_destroy nodePath
  return transform
  where getViewportCamera :: GodotSimulaServer -> IO GodotCamera
        getViewportCamera gss = do
          viewport <- G.get_viewport gss :: IO GodotViewport
          camera <- G.get_camera viewport :: IO GodotCamera
          return camera

showGSVS :: GodotSimulaViewSprite -> IO String
showGSVS gsvs = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let maybeUUID = (simulaView ^. gsvsUUID)
  let ret = case maybeUUID of
                  Nothing -> "(GSVS: Nothing)"
                  Just uuid -> "(GSVS: " ++ (show uuid) ++ ")"
  return ret

logGSVS :: String -> GodotSimulaViewSprite -> IO ()
logGSVS str gsvs = do
  appendFile "log.txt" $ "Printing from " ++ (show str) ++ "\n"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  isMapped <- readTVarIO (simulaView ^. svMapped)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  let maybeID = (simulaView ^. gsvsUUID)
  case maybeID of
     Nothing -> appendFile "log.txt" $  "  Couldn't get GSVS ID" ++ "\n"
     (Just id) -> do appendFile "log.txt" $ "  gsvs id: " ++ (show id) ++ "\n"
                     case eitherSurface of
                       Left wlrXdgSurface -> do appendFile "log.txt" $ ("  wlrXdgSurface: " ++ (show wlrXdgSurface)) ++ "\n"
                                                wlrSurface <- G.get_wlr_surface wlrXdgSurface
                                                appendFile "log.txt" $ ("  wlrSurface: " ++ (show wlrSurface)) ++ "\n"
                       Right wlrXWaylandSurface -> do appendFile "log.txt" $ ("  wlrXWaylandSurface: " ++ (show wlrXWaylandSurface)) ++ "\n"
                                                      wlrSurface <- G.get_wlr_surface wlrXWaylandSurface
                                                      appendFile "log.txt" $ ("  wlrSurface: " ++ (show wlrSurface)) ++ "\n"
  appendFile "log.txt" $ "  isMapped: " ++ (show isMapped) ++ "\n"

logStr :: String -> IO ()
logStr string = do
  appendFile "./log/log.txt" $ string ++ "\n"

logPutStrLn :: String -> IO ()
logPutStrLn string = do
  logStr string
  putStrLn string

-- | returns Just pid or Nothing if process has already exited
getPid :: ProcessHandle -> IO (Maybe ProcessID)
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing

-- Generic imports from SimulaCanvasItem.hs
getCoordinatesFromCenter :: GodotWlrSurface -> Int -> Int -> IO GodotVector2
getCoordinatesFromCenter wlrSurface sx sy = do
  -- putStrLn "getCoordinatesFromCenter"
  (bufferWidth', bufferHeight')    <- getBufferDimensions wlrSurface
  let (bufferWidth, bufferHeight)  = (fromIntegral bufferWidth', fromIntegral bufferHeight')
  let (fromTopLeftX, fromTopLeftY) = (fromIntegral sx, fromIntegral sy)
  let fromCenterX                  = -(bufferWidth/2) + fromTopLeftX
  let fromCenterY                  = -(-(bufferHeight/2) + fromTopLeftY)
  -- NOTE: In godotston fromCenterY is isn't negative, but since we set
  -- `G.render_target_v_flip viewport True` we can set this
  -- appropriately
  let v2 = (V2 fromCenterX fromCenterY) :: V2 Float
  gv2 <- toLowLevel v2 :: IO GodotVector2
  return gv2

-- Delete improveTextureQuality
-- Delete old intializeRenderTarget

data ViewportType = ViewportBase | ViewportSurface

initializeRenderTarget :: GodotSimulaViewSprite -> ViewportType -> IO (GodotViewport)
initializeRenderTarget gsvs viewportType = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurface <- getWlrSurface eitherSurface
  renderTarget <- unsafeInstance GodotViewport "Viewport"
  dimensions@(width, height) <- getBufferDimensions wlrSurface
  pixelDimensionsOfWlrSurface <- toGodotVector2 dimensions

  G.set_disable_input renderTarget True
  G.set_usage renderTarget G.USAGE_2D
  G.set_update_mode renderTarget G.UPDATE_WHEN_VISIBLE
  G.set_vflip renderTarget True
  G.set_size renderTarget pixelDimensionsOfWlrSurface

  case viewportType of
    ViewportSurface -> G.set_clear_mode renderTarget G.CLEAR_MODE_NEVER
    ViewportBase -> G.set_clear_mode renderTarget G.CLEAR_MODE_ALWAYS
  G.set_transparent_background renderTarget True

  return renderTarget

getBufferDimensions :: GodotWlrSurface -> IO (Int, Int)
getBufferDimensions wlrSurface = do
  dims@(bufferWidth, bufferHeight) <- withCurrentState $ getBufferDimensions'
  return dims
  where withCurrentState :: (GodotWlrSurfaceState -> IO b) -> IO b
        withCurrentState stateAction = do
          wlrSurfaceState <- G.alloc_current_state wlrSurface
          ret             <- stateAction wlrSurfaceState
          G.delete_state wlrSurfaceState
          return ret
        getBufferDimensions' :: GodotWlrSurfaceState -> IO (Int, Int)
        getBufferDimensions' wlrSurfaceState = do
          -- bufferWidth <- G.get_buffer_width wlrSurfaceState
          -- bufferHeight <- G.get_buffer_height wlrSurfaceState
          -- return (bufferWidth, bufferHeight)
          width <- G.get_width wlrSurfaceState
          height <-G.get_height wlrSurfaceState
          return (width, height)

newCanvasSurface :: GodotSimulaViewSprite -> IO CanvasSurface
newCanvasSurface gsvs = do
  cs <- "res://addons/godot-haskell-plugin/CanvasSurface.gdns"
    & newNS' []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr :: IO CanvasSurface
  viewport <- initializeRenderTarget gsvs ViewportSurface

  addChild gsvs viewport
  addChild viewport cs

  atomically $ writeTVar (_csGSVS cs) gsvs
  atomically $ writeTVar (_csViewport cs) viewport

  atomically $ writeTVar (_csFrameCounter cs) 0

  return cs

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

savePng :: CanvasSurface -> GodotViewportTexture -> GodotWlrSurface -> IO String
savePng cs surfaceTexture wlrSurface = do
  validateSurfaceE wlrSurface
  -- Get image
  gsvs <- readTVarIO (cs ^. csGSVS)
  surfaceTextureAsImage <- G.get_data surfaceTexture

  -- Get file path
  frame <- readTVarIO (gsvs ^. gsvsFrameCount)
  createDirectoryIfMissing False "media"
  let pathStr = "./media/" ++ (show (coerce wlrSurface :: Ptr GodotWlrSurface)) ++ "." ++ (show frame) ++ ".png"
  canonicalPath <- canonicalizePath pathStr
  pathStr' <- toLowLevel (pack pathStr)

  -- Save as png
  G.save_png surfaceTextureAsImage pathStr'
  return canonicalPath
  where getVisualServer gsvs = do
          gss <- readTVarIO (gsvs ^. gsvsServer)
          visualServer <- readTVarIO (gss ^. gssVisualServer)
          return visualServer

type ScreenshotBaseName = String
type ScreenshotFullPath = String

savePngPancake :: GodotSimulaServer -> ScreenshotBaseName -> IO (ScreenshotFullPath)
savePngPancake gss screenshotBaseName = do
  viewport <- G.get_viewport gss :: IO GodotViewport
  viewportTexture <- G.get_texture viewport
  pancakeImg <- G.get_data viewportTexture
  G.flip_y pancakeImg
  createDirectoryIfMissing False "media"
  let relativePath = ("./media/" <> screenshotBaseName <> ".png")
  fullPath <- System.Directory.canonicalizePath relativePath
  G.save_png pancakeImg =<< toLowLevel (pack relativePath)
  return fullPath

-- Run shell command with DISPLAY set to our XWayland server value (typically
-- :2)
appLaunch :: GodotSimulaServer -> String -> Maybe String -> IO ProcessID
appLaunch gss appStr maybeLocation = do
  let originalEnv = (gss ^. gssOriginalEnv)
  maybeXwaylandDisplay <- readTVarIO (gss ^. gssXWaylandDisplay)
  case (appStr, maybeXwaylandDisplay) of
        ("nullApp", _) -> do return $ fromInteger 0
        ("launchHMDWebcam", _) -> launchHMDWebCam gss maybeLocation
        ("launchTerminal", _) -> terminalLaunch gss maybeLocation
        ("launchUsageInstructions", _) -> appLaunch gss "./result/bin/midori https://github.com/SimulaVR/Simula#usage -p" maybeLocation
        (_, Nothing) -> putStrLn "No DISPLAY found!" >> (return $ fromInteger 0)
        (_, (Just xwaylandDisplay)) -> do
            let envMap = M.fromList originalEnv
            let envMapWithDisplay = M.insert "DISPLAY" xwaylandDisplay envMap
            let envMapWithWaylandDisplay = case maybeLocation of
                                                Just location -> M.insert "SIMULA_STARTING_LOCATION" location (M.insert "WAYLAND_DISPLAY" "simula-0" envMapWithDisplay)
                                                Nothing -> (M.insert "WAYLAND_DISPLAY" "simula-0" envMapWithDisplay)
            let envListWithDisplays = M.toList envMapWithWaylandDisplay
            res <- Control.Exception.Safe.try $ createProcess (shell appStr) { env = Just envListWithDisplays, new_session = True } :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
            pid <- case res of
                        Left _ -> return $ fromInteger 0
                        Right (_, _, _, processHandle) -> do maybePid <- System.Process.getPid processHandle
                                                             case maybePid of
                                                                  Just pid -> return pid
                                                                  Nothing -> return $ fromInteger $ 0
            startingAppPids <- readTVarIO (gss ^. gssStartingAppPids)
            case maybeLocation of
              Nothing -> return ()
              Just location -> do let startingAppPids' = M.insertWith (++) pid [location] startingAppPids
                                  atomically $ writeTVar (gss ^. gssStartingAppPids) startingAppPids'
            return pid

launchHMDWebCam :: GodotSimulaServer -> Maybe String -> IO ProcessID
launchHMDWebCam gss maybeLocation = do
  maybePath <- getHMDWebCamPath
  case maybePath of
    Nothing -> do putStrLn "Cannot find HMD web cam!"
                  appLaunch gss "nullApp" Nothing
    Just path  -> appLaunch gss ("./result/bin/ffplay -loglevel quiet -f v4l2" ++ path) maybeLocation
    where getHMDWebCamPath :: IO (Maybe FilePath)
          getHMDWebCamPath = do
            res <- Control.Exception.Safe.try $ listDirectory "/dev/v4l/by-id" :: IO (Either IOException [FilePath])
            case res of
              Left _ -> return Nothing
              Right _ -> (listToMaybe . Data.List.map ("/dev/v4l/by-id/" ++) . sort . Data.List.filter viveOrValve) <$> listDirectory "/dev/v4l/by-id"
          viveOrValve :: String -> Bool
          viveOrValve str = Data.List.any (`Data.List.isInfixOf` str) ["Vive",  -- HTC Vive
                                                                       "VIVE",  -- HTC Vive Pro
                                                                       "Valve", -- Valve Index?
                                                                       "Etron"] -- Valve Index
terminalLaunch :: GodotSimulaServer -> Maybe String -> IO ProcessID
terminalLaunch gss maybeLocation = appLaunch gss "./result/bin/xfce4-terminal" maybeLocation

getTextureFromURL :: String -> IO (Maybe GodotTexture)
getTextureFromURL urlStr = do
   godotImage <- unsafeInstance GodotImage "Image" :: IO GodotImage
   godotImageTexture <- unsafeInstance GodotImageTexture "ImageTexture"
   pngUrl <- toLowLevel (pack urlStr) :: IO GodotString
   exitCode <- G.load godotImage pngUrl
   -- G.compress godotImage G.COMPRESS_ETC2 G.COMPRESS_SOURCE_GENERIC 1
   G.create_from_image godotImageTexture godotImage G.TEXTURE_FLAGS_DEFAULT
   Api.godot_string_destroy pngUrl
   Api.godot_object_destroy $ safeCast godotImage
   if (unsafeCoerce godotImageTexture == nullPtr) then (return Nothing) else (return (Just (safeCast godotImageTexture)))

loadEnvironmentTextures :: Configuration -> GodotWorldEnvironment -> IO [String]
loadEnvironmentTextures configuration worldEnvironment = do
  -- configuration <- readTVarIO (gss ^. gssConfiguration)
  let envDir = (configuration ^. environmentsDirectory)
  dirExists <- Control.Exception.Safe.try $ listDirectory envDir :: IO (Either IOException [FilePath])
  dirs <- case dirExists of
              Left _ -> return []
              Right _ -> findAll [".png", ".jpg", ".jpeg"] <$> listDirectory envDir
  let dirs' = Data.List.map (("./" ++ envDir ++ "/") ++) dirs
  -- textures <- catMaybes <$> mapM (getEnvironmentTexture (worldEnvironment, tex)) dirs'
  return dirs'
  where contain w end = Data.List.take (Data.List.length end) (Data.List.reverse w) == Data.List.reverse end
        findAll ends txt = Data.List.filter (\w -> Data.List.any (contain w) ends) txt

getEnvironmentTexture :: GodotWorldEnvironment -> String -> IO (Maybe GodotTexture)
getEnvironmentTexture worldEnvironment filePath = do
  -- (worldEnvironment, tex) <- readTVarIO (gss ^. gssWorldEnvironment)
  environment <- G.get_environment worldEnvironment :: IO GodotEnvironment
  sky <- G.get_sky environment :: IO GodotSky
  panoramaSky <- asClass' GodotPanoramaSky "PanoramaSky" sky

  maybeMilkyTexture <- getTextureFromURL $ "res://" ++ filePath
  -- maybeMilkyTexture <- getTextureFromURL $ filePath
  case maybeMilkyTexture of
    Nothing -> do putStrLn $ "Can't load texture: " ++ filePath
                  return Nothing
    Just tex -> return $ Just tex

instance Eq GodotTexture where
  texture1 == texture2 = ((coerce texture1) :: Ptr ()) == ((coerce texture2) :: Ptr ())

next :: Eq a => Maybe a -> [a] -> Maybe a
next _ []             = Nothing
next Nothing    (x:_) = Just x
next (Just e) l@(x:_) = Just $ case Data.List.dropWhile (/= e) l of
                          (_:y:_) -> y
                          _       -> x

-- TODO: This leaks. Fix it.
cycleGSSEnvironment :: GodotSimulaServer -> IO ()
cycleGSSEnvironment gss = do
  (worldEnvironment, currentTextureStr) <- readTVarIO (gss ^. gssWorldEnvironment)
  texturesLst <- readTVarIO (gss ^. gssEnvironmentTextures)
  environment <- G.get_environment worldEnvironment :: IO GodotEnvironment
  sky <- G.get_sky environment :: IO GodotSky
  panoramaSky <- asClass' GodotPanoramaSky "PanoramaSky" sky

  let maybeNextTextureStr = next (Just currentTextureStr) texturesLst
  case maybeNextTextureStr of
    Nothing -> putStrLn $ "Unable to cycle environment texture!"
    Just nextTextureStr -> do
      atomically $ writeTVar (gss ^. gssWorldEnvironment) (worldEnvironment, nextTextureStr)
      maybeNextTexture <- getEnvironmentTexture worldEnvironment nextTextureStr
      case maybeNextTexture of
        Nothing -> putStrLn "Unable to cycle environment texture!"
        Just nextTexture -> do oldTex <- G.get_panorama panoramaSky :: IO GodotTexture
                               G.set_panorama panoramaSky nextTexture
                               -- G.unreference @GodotReference (safeCast oldTex) -- Doesn't work here
                               Api.godot_object_destroy $ safeCast oldTex
                               return ()

cycleGSSScene :: GodotSimulaServer -> IO ()
cycleGSSScene gss = do
  maybeCurrentScene <- readTVarIO (gss ^. gssScene)
  scenes <- readTVarIO (gss ^. gssScenes)
  maybeNextSceneStr <- case maybeCurrentScene of
    Nothing -> do
      let maybeHeadSceneStr = head' scenes
      return $ maybeHeadSceneStr
    Just (currentSceneStr, currentSceneNode) -> do
      let maybeNextSceneStr = next (Just currentSceneStr) scenes
      return maybeNextSceneStr
  maybeNextSceneNode <- case maybeNextSceneStr of
                             Nothing -> do putStrLn "Unable to change scenes!"
                                           return Nothing
                             Just nextSceneStr -> do
                                  resourceLoader <- getSingleton Godot_ResourceLoader "ResourceLoader"
                                  nextScenePath <- toLowLevel (pack nextSceneStr)
                                  typeHint <- toLowLevel ""
                                  (GodotResource nextSceneObj) <- G.load resourceLoader nextScenePath typeHint False
                                  let nextScenePacked = GodotPackedScene nextSceneObj
                                  nextSceneInstance <- G.instance' nextScenePacked 0
                                  return $ Just nextSceneInstance
  case (maybeCurrentScene, maybeNextSceneStr, maybeNextSceneNode) of
    (Nothing, Just nextSceneStr, Just nextSceneNode) -> do
      addChild gss nextSceneNode
      atomically $ writeTVar (gss ^. gssScene) $ Just (nextSceneStr, nextSceneNode)
    (Just (currentSceneStr, currentSceneNode), Just nextSceneStr, Just nextSceneNode) -> do
      removeChild gss currentSceneNode
      queueFreeNodeAndChildren currentSceneNode
      addChild gss nextSceneNode
      atomically $ writeTVar (gss ^. gssScene) $ Just (nextSceneStr, nextSceneNode)
    _ -> putStrLn "Unable to change scenes!"
  where head' :: [a] -> Maybe a
        head' [] = Nothing
        head' (x:xs) = Just x

orientSpriteTowardsGaze :: GodotSimulaViewSprite -> IO ()
orientSpriteTowardsGaze gsvs = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  isInSceneGraph <- G.is_a_parent_of ((safeCast gss) :: GodotNode ) ((safeCast gsvs) :: GodotNode)
  case isInSceneGraph of
    False -> putStrLn "Nothing to orient!"
    True -> do gss <- readTVarIO (gsvs ^. gsvsServer)
               upV3 <- toLowLevel (V3 0 1 0) :: IO GodotVector3
               rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
               targetV3 <- getARVRCameraOrPancakeCameraTransformGlobal gss >>= Api.godot_transform_get_origin -- void look_at ( Vector3 target, Vector3 up )
               G.look_at gsvs targetV3 upV3                      -- The negative z-axis of the gsvs looks at HMD
               return ()
               atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True -- Useful debugging hack to force gsvs to redraw
               -- G.rotate_object_local gsvs rotationAxisY 3.14159  -- The positive z-axis of the gsvs looks at HMD


addLeapMotionScene :: GodotSimulaServer -> IO ()
addLeapMotionScene gss = do
  resourceLoader <- getSingleton Godot_ResourceLoader "ResourceLoader"
  nextScenePath <- toLowLevel (pack "res://addons/gdleapmotion/scenes/leap_motion.tscn")
  typeHint <- toLowLevel ""
  (GodotResource nextSceneObj) <- G.load resourceLoader nextScenePath typeHint False
  let nextScenePacked = GodotPackedScene nextSceneObj
  nextSceneInstance <- G.instance' nextScenePacked 0
  addChild gss nextSceneInstance

resizeGSVS :: GodotSimulaViewSprite -> ResizeMethod -> Float -> IO ()
resizeGSVS gsvs resizeMethod factor =
  do maybeOldTargetDims <- readTVarIO (gsvs ^. gsvsTargetSize)
     oldTargetDims@(SpriteDimensions (w, h)) <- case maybeOldTargetDims of
       Just oldTargetDims' -> return oldTargetDims'
       Nothing -> do simulaView <- readTVarIO (gsvs ^. gsvsView)
                     let eitherSurface = (simulaView ^. svWlrEitherSurface)
                     wlrSurface <- getWlrSurface eitherSurface
                     (x, y) <- getBufferDimensions wlrSurface
                     return $ SpriteDimensions (x, y)

     newTargetDims@(SpriteDimensions (wTarget, hTarget)) <- case resizeMethod of
            Horizontal -> do
              case (((fromIntegral w) * factor) > 500) of
                True -> do -- orientSpriteTowardsGaze gsvs -- Avoid quadMesh rotational behavior (see `setTargetDimensions`)
                           return $ SpriteDimensions (round $ ((fromIntegral w) * factor), round $ ((fromIntegral h)))
                False -> return $ oldTargetDims
            Vertical   -> do
              case (((fromIntegral h) * factor) > 500) of
                True -> do -- orientSpriteTowardsGaze gsvs -- Avoid quadMesh rotational behavior (see `setTargetDimensions`)
                           return $ SpriteDimensions (round $ ((fromIntegral w)), round $ ((fromIntegral h) * factor))
                False -> return $ oldTargetDims
            Zoom       -> do
              case (((fromIntegral h) * factor) > 500) && (((fromIntegral h) * factor)  < 1500) of
                False -> return $ oldTargetDims
                True -> do V3 1 1 1 ^* (1 + 1 * (1 - factor)) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)
                           return $ SpriteDimensions (round $ ((fromIntegral w) * factor), round $ ((fromIntegral h) * factor))

     atomically $ do if resizeMethod == Zoom then return () else writeTVar (gsvs ^. gsvsResizedLastFrame) True
                     writeTVar (gsvs ^. gsvsTargetSize) (Just newTargetDims)
                     writeTVar (gsvs ^. gsvsSpilloverDims) (Just (wTarget, hTarget))
                     writeTVar (gsvs ^. gsvsIsDamaged) True

defaultSizeGSVS :: GodotSimulaViewSprite -> IO ()
defaultSizeGSVS gsvs = do
    gss <- readTVarIO (gsvs ^. gsvsServer)
    configuration <- readTVarIO (gss ^. gssConfiguration)
    let windowScale = realToFrac (configuration ^. defaultWindowScale) :: Float
    (V3 1 1 1 ^* (windowScale + 1)) & toLowLevel >>= G.set_scale (safeCast gsvs :: GodotSpatial)

    let maybeWindowResolution = (configuration ^. defaultWindowResolution) :: Maybe (Dhall.Natural, Dhall.Natural)
    let newTargetDims@(SpriteDimensions (x, y)) = case maybeWindowResolution of
                                                       Just windowResolution'@(x, y) ->  SpriteDimensions (fromIntegral x, fromIntegral y)
                                                       Nothing -> SpriteDimensions (900, 900)

    atomically $ do writeTVar (gsvs ^. gsvsTargetSize) (Just newTargetDims)
                    writeTVar (gsvs ^. gsvsIsDamaged) True

getQuadMesh :: GodotSimulaViewSprite -> IO GodotQuadMesh
getQuadMesh gsvs = do
  meshInstance <- readTVarIO (gsvs ^. gsvsMeshInstance)
  quadMesh <- G.get_mesh meshInstance >>= asClass' GodotQuadMesh "QuadMesh"
  return quadMesh

constrainTransparency :: Float -> Float
constrainTransparency input =
  case ((input < 0.0), (input > 1.0)) of
        (True, _) -> 0.0
        (_, True) -> 1.0
        _ -> input

saveViewportAsPngAndLaunch :: GodotSimulaViewSprite -> GodotViewportTexture -> M22 Float -> IO ()
saveViewportAsPngAndLaunch gsvs tex m22@(V2 (V2 ox oy) (V2 ex ey)) = do
  let isNull = ((unsafeCoerce tex) == nullPtr)
  case isNull of
    True -> putStrLn "Texture is null in saveViewportAsPngAndLaunch!"
    False -> do -- Get image
                texAsImage <- G.get_data tex

                -- Get file path
                timeStampStr <- show <$> getCurrentTime
                createDirectoryIfMissing False "media"
                let pathStr = "./media/" ++  ((Data.List.filter (/= '"') . show) timeStampStr) ++ ".png"
                pathStr' <- toLowLevel (pack pathStr)

                -- Save as png
                rect <- toLowLevel m22
                rectImage <- G.get_rect texAsImage rect
                System.Directory.createDirectoryIfMissing True "png"
                G.save_png rectImage pathStr'
                gss <- readTVarIO (gsvs ^. gsvsServer)

                -- Copy to clipboard
                appLaunch gss ("./result/bin/xclip -selection clipboard -t image/png -i" ++ pathStr) Nothing >> return ()

                return ()

fromGodotArray :: GodotArray -> IO [GodotVariant]
fromGodotArray vs = do
  size <- fromIntegral <$> Api.godot_array_size vs
  forM [0..size-1] $ Api.godot_array_get vs

getDepthFirstSurfaces :: GodotSimulaViewSprite -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstSurfaces gsvs = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurfaceParent <- (getWlrSurface eitherSurface) >>= validateSurfaceE
  depthFirstBaseSurfaces <- getDepthFirstBaseSurfaces gsvs
  depthFirstWlrSurfaces <- getDepthFirstWlrSurfaces wlrSurfaceParent
  let depthFirstSurfaces = depthFirstBaseSurfaces ++ depthFirstWlrSurfaces
  return depthFirstSurfaces

getDepthFirstBaseSurfaces :: GodotSimulaViewSprite -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstBaseSurfaces gsvs = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurfaceParent <- getWlrSurface eitherSurface
  depthFirstBaseSurfaces <- case eitherSurface of
    Left wlrXdgSurface -> do
      ret <- getDepthFirstXdgSurfaces wlrXdgSurface :: IO [(GodotWlrSurface, Int, Int)]
      return ret
    Right wlrXWaylandSurface -> do
      freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
      freeChildren' <- mapM (\wlrXWaylandSurfaceFC -> do x <- G.get_x wlrXWaylandSurfaceFC
                                                         y <- G.get_y wlrXWaylandSurfaceFC
                                                         wlrSurface <- G.get_wlr_surface wlrXWaylandSurfaceFC
                                                         return (wlrSurface, x, y))
                            freeChildren
      depthFirstXWaylandSurfaces <- getDepthFirstXWaylandSurfaces wlrXWaylandSurface :: IO [(GodotWlrSurface, Int, Int)]
      return (depthFirstXWaylandSurfaces ++ freeChildren')
  return depthFirstBaseSurfaces

-- TODO: All (Int, Int) should be relative to root surface; right now,
-- subsurface coordinates are possibly relative to their immediate parent.
getDepthFirstXWaylandSurfaces :: GodotWlrXWaylandSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstXWaylandSurfaces wlrXWaylandSurface = do
  xwaylandMappedChildrenAndCoords <- getXWaylandMappedChildren wlrXWaylandSurface :: IO [(GodotWlrXWaylandSurface, Int, Int)]
  wlrSurface <- G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface
  foldM appendXWaylandSurfaceAndChildren [(wlrSurface, 0, 0)] xwaylandMappedChildrenAndCoords
  where
        appendXWaylandSurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrXWaylandSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        appendXWaylandSurfaceAndChildren oldList arg@(wlrXWaylandSurface, x, y) = do
           xwaylandChildSurface <- G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface
           appendSurfaceAndChildren oldList (xwaylandChildSurface, x, y)

        appendSurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        appendSurfaceAndChildren oldList arg@(wlrSurface, x, y) = do
           subsurfacesAndCoords <- getSurfaceChildren wlrSurface :: IO [(GodotWlrSurface, Int, Int)]
           foldM appendSurfaceAndChildren (oldList ++ [(wlrSurface, x, y)]) subsurfacesAndCoords

        getSurfaceChildren :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
        getSurfaceChildren wlrSurface = do
          arrayOfChildren <- G.get_children wlrSurface :: IO GodotArray
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          childrenSubsurfaces <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrSubsurface]
          childrenSSX <- mapM G.get_ssx childrenSubsurfaces
          childrenSSY <- mapM G.get_ssy childrenSubsurfaces
          children <- mapM G.getWlrSurface childrenSubsurfaces
          let childrenWithCoords = zip3 children childrenSSX childrenSSY
          Api.godot_array_destroy arrayOfChildren
          mapM_ Api.godot_variant_destroy arrayOfChildrenGV
          return childrenWithCoords

        getXWaylandMappedChildren :: GodotWlrXWaylandSurface -> IO [(GodotWlrXWaylandSurface, Int, Int)]
        getXWaylandMappedChildren wlrXWaylandSurface = do
          arrayOfChildren <- G.get_children wlrXWaylandSurface :: IO GodotArray -- Doesn't return non-mapped children
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXWaylandSurface]
          childrenX <- mapM G.get_x children
          childrenY <- mapM G.get_y children
          let childrenWithCoords = zip3 children childrenX childrenY
          Api.godot_array_destroy arrayOfChildren
          mapM_ Api.godot_variant_destroy arrayOfChildrenGV
          return childrenWithCoords

getDepthFirstXdgSurfaces :: GodotWlrXdgSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstXdgSurfaces wlrXdgSurface = do
  xdgPopups <- getXdgPopups wlrXdgSurface :: IO [(GodotWlrXdgSurface, Int, Int)]
  depthFirstXdgSurfaces  <- foldM appendXdgSurfaceAndChildren [(wlrXdgSurface, 0, 0)] xdgPopups
  mapM convertToWlrSurfaceDepthFirstSurfaces depthFirstXdgSurfaces
  where convertToWlrSurfaceDepthFirstSurfaces :: (GodotWlrXdgSurface, Int, Int) -> IO (GodotWlrSurface, Int, Int)
        convertToWlrSurfaceDepthFirstSurfaces (wlrXdgSurface, x, y) = do
          wlrSurface <- G.get_wlr_surface wlrXdgSurface
          return (wlrSurface, x, y)

        getXdgPopups :: GodotWlrXdgSurface -> IO [(GodotWlrXdgSurface, Int, Int)]
        getXdgPopups wlrXdgSurface = do
          arrayOfChildren <- G.get_children wlrXdgSurface :: IO GodotArray -- Doesn't return non-mapped children
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXdgSurface]
          childrenAsPopups <- mapM G.get_xdg_popup children
          childrenX <- mapM G.get_x childrenAsPopups
          childrenY <- mapM G.get_y childrenAsPopups
          let childrenWithCoords = zip3 children childrenX childrenY
          Api.godot_array_destroy arrayOfChildren
          mapM_ Api.godot_variant_destroy arrayOfChildrenGV
          return childrenWithCoords

        appendXdgSurfaceAndChildren :: [(GodotWlrXdgSurface, Int, Int)] -> (GodotWlrXdgSurface, Int, Int) -> IO [(GodotWlrXdgSurface, Int, Int)]
        appendXdgSurfaceAndChildren oldList arg@(wlrXdgSurface, x, y) = do
          subsurfacesAndCoords <- getXdgPopups wlrXdgSurface :: IO [(GodotWlrXdgSurface, Int, Int)]
          foldM appendXdgSurfaceAndChildren (oldList ++ [(wlrXdgSurface, x, y)]) subsurfacesAndCoords

getDepthFirstWlrSurfaces :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
getDepthFirstWlrSurfaces wlrSurface = do
  surfaceChildrenAndCoords <- getSurfaceChildren wlrSurface
  foldM appendSurfaceAndChildren [] surfaceChildrenAndCoords
  where appendSurfaceAndChildren :: [(GodotWlrSurface, Int, Int)] -> (GodotWlrSurface, Int, Int) -> IO [(GodotWlrSurface, Int, Int)]
        appendSurfaceAndChildren oldList arg@(wlrSurface, x, y) = do
          surfacesAndCoords <- getSurfaceChildren wlrSurface :: IO [(GodotWlrSurface, Int, Int)]
          foldM appendSurfaceAndChildren (oldList ++ [(wlrSurface, x, y)]) surfacesAndCoords

        getSurfaceChildren :: GodotWlrSurface -> IO [(GodotWlrSurface, Int, Int)]
        getSurfaceChildren wlrSurface = do
          arrayOfChildren <- G.get_children wlrSurface :: IO GodotArray
          numChildren <- Api.godot_array_size arrayOfChildren
          arrayOfChildrenGV <- fromGodotArray arrayOfChildren
          childrenSubsurfaces <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrSubsurface]
          childrenSSX <- mapM G.get_ssx childrenSubsurfaces
          childrenSSY <- mapM G.get_ssy childrenSubsurfaces
          children <- mapM G.getWlrSurface childrenSubsurfaces
          let childrenWithCoords = zip3 children childrenSSX childrenSSY
          Api.godot_array_destroy arrayOfChildren
          mapM_ Api.godot_variant_destroy arrayOfChildrenGV
          return childrenWithCoords


-- Types.hs
getBaseDimensions :: GodotSimulaViewSprite -> IO (Int, Int)
getBaseDimensions gsvs = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurfaceParent <- getWlrSurface eitherSurface
  (parentWidth, parentHeight) <- getBufferDimensions wlrSurfaceParent
  return (parentWidth, parentHeight)

getSpilloverDims :: GodotSimulaViewSprite -> IO (Int, Int)
getSpilloverDims gsvs = do
  depthFirstSurfaces <- getDepthFirstSurfaces gsvs :: IO [(GodotWlrSurface, Int, Int)]
  spilloverDims <- mapM (getSpilloverDims gsvs) depthFirstSurfaces
  let spilloverWidth = Data.List.maximum $ fmap fst spilloverDims
  let spilloverHeight = Data.List.maximum $ fmap snd spilloverDims
  return (spilloverWidth, spilloverHeight)
  where getSpilloverDims :: GodotSimulaViewSprite -> (GodotWlrSurface, Int, Int) -> IO (Int, Int)
        getSpilloverDims gsvs (wlrSurface, sx, sy) = do
          (baseWidth, baseHeight) <- getBaseDimensions gsvs
          (childWidth, childHeight) <- getBufferDimensions wlrSurface
          let widthSpill = max 0 ((sx + childWidth) - baseWidth)
          let heightSpill = max 0 ((sy + childHeight) - baseHeight)
          return $ (baseWidth + widthSpill, baseHeight + heightSpill)

setShader :: GodotSimulaViewSprite -> String -> IO ()
setShader gsvs tres = do
  putStrLn $ "setShader: " ++ tres
  quadMesh <- getQuadMesh gsvs
  shader <- load GodotShader "Shader" (Data.Text.pack tres)
  case shader of
    Just shader -> do
      shm <- unsafeInstance GodotShaderMaterial "ShaderMaterial"
      G.set_shader shm shader
      G.set_material quadMesh (safeCast shm)
    Nothing -> error "couldn't fetch shader"

getParentPid :: ProcessID -> IO (Maybe ProcessID)
getParentPid pid = do
  let fp = "/proc/" ++ show (toInteger pid) ++ "/stat"
  econtents <- Control.Exception.Safe.try $ readFile fp :: IO (Either SomeException String)
  case econtents of
    Right xs ->
      case Data.List.lines xs of
        [ws] -> case Data.List.words ws of
          (_:_:_:ppid:_) -> return . Just . Prelude.read $ ppid
    _ -> return Nothing

getParentsPids :: ProcessID  -> IO [ProcessID]
getParentsPids pid = do
  ppid <- getParentPid pid
  case ppid of
    Just ppid' -> do
      ps <- getParentsPids ppid'
      return (ppid':ps)
    Nothing -> return []

getSimulaStartingLocationAtomically :: GodotSimulaServer -> [ProcessID] -> IO (Maybe String)
getSimulaStartingLocationAtomically gss pids = do
  ret <- case pids of
              [] -> return Nothing
              pid:pids -> atomically $ do startingAppPids <- readTVar (gss ^. gssStartingAppPids)
                                          case (M.lookup pid startingAppPids) of
                                            Nothing -> do
                                              return Nothing
                                            Just [] -> do
                                              let startingAppPids' = M.delete pid startingAppPids
                                              writeTVar (gss ^. gssStartingAppPids) startingAppPids'
                                              return Nothing
                                            Just (location:locations) -> do
                                              let startingAppPids' = M.insert pid locations startingAppPids
                                              writeTVar (gss ^. gssStartingAppPids) startingAppPids'
                                              return $ (Just location)

  case (pids, ret) of
       (_, Just ret) -> return $ Just ret
       ([], Nothing) -> return Nothing
       (pid:pids, Nothing) -> do
         getSimulaStartingLocationAtomically gss pids

keyboardGrabInitiate :: GodotSimulaServer -> Grab -> IO ()
keyboardGrabInitiate gss (GrabWindow gsvs _) = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  -- TODO: Ensure that we aren't keyboard grabbing other stuff, w/o messing up diff state
  -- keyboardGrabLetGo (GrabWorkspaces gss)

  simulaView <- readTVarIO (gsvs ^. gsvsView)
  isInSceneGraph <- G.is_a_parent_of ((safeCast gss) :: GodotNode ) ((safeCast gsvs) :: GodotNode)
  case isInSceneGraph of
    False -> keyboardGrabLetGo gss (GrabWindow gsvs undefined)
    True -> do gss <- readTVarIO $ (gsvs ^. gsvsServer)
               -- Compute dist
               orientSpriteTowardsGaze gsvs
               posGSVS <- (G.get_global_transform gsvs) >>= Api.godot_transform_get_origin
               hmdTransform <- getARVRCameraOrPancakeCameraTransformGlobal gss
               posHMD  <- Api.godot_transform_get_origin hmdTransform
               dist <- realToFrac <$> Api.godot_vector3_distance_to posGSVS posHMD
               -- Load state
               atomically $ writeTVar (gss ^. gssGrab) (Just (GrabWindow gsvs (-dist)))
  return ()
keyboardGrabInitiate gss (GrabWindows _) = do
  -- TODO: Ensure that we aren't keyboard grabbing other workspaces, w/o messing up diff state
  -- keyboardGrabLetGo gss GrabWorkspaces

  povTransform <- getARVRCameraOrPancakeCameraTransform gss
  atomically $ writeTVar (gss ^. gssGrab) (Just (GrabWindows povTransform))

keyboardGrabInitiate gss (GrabWorkspaces _)  = do
  -- TODO: Ensure that we aren't keyboard grabbing anything else, w/o messing up diff state
  -- keyboardGrabLetGo gss (GrabWorkspaces _)

  povTransform <- getARVRCameraOrPancakeCameraTransform gss
  atomically $ writeTVar (gss ^. gssGrab) (Just (GrabWorkspaces povTransform))

keyboardGrabLetGo :: GodotSimulaServer -> Grab -> IO ()
keyboardGrabLetGo gss (GrabWindow gsvs _)  = do
  gss <- readTVarIO $ (gsvs ^. gsvsServer)
  atomically $ writeTVar (gss ^. gssGrab) Nothing
keyboardGrabLetGo gss (GrabWindows _) = do
  (currentWorkspace, currentWorkspaceStr) <- readTVarIO (gss ^. gssWorkspace)
  workspacePersistent <- readTVarIO (gss ^. gssWorkspacePersistent)
  currentWorkspaceTransform <- G.get_transform currentWorkspace
  workspacePersistentTransform <- G.get_transform workspacePersistent
  updateDiffMap gss currentWorkspace currentWorkspaceTransform
  updateDiffMap gss workspacePersistent workspacePersistentTransform
  atomically $ writeTVar (gss ^. gssGrab) Nothing

keyboardGrabLetGo gss (GrabWorkspaces _) = do
  gssTransform <- G.get_transform gss
  updateDiffMap gss (safeCast gss) gssTransform
  atomically $ writeTVar (gss ^. gssGrab) Nothing

getGrabDiff :: GodotSimulaServer -> IO GodotTransform
getGrabDiff gss = do
  maybeGrab <- readTVarIO (gss ^. gssGrab)
  diffTransform  <- case maybeGrab of
                         Nothing -> do let idTransform = TF (identity :: M33 Float) (V3 0 0 0)
                                       toLowLevel idTransform
                         (Just (GrabWindows prevTransform)) -> do prevTransformInverse <- Api.godot_transform_affine_inverse prevTransform
                                                                  povTransform <- getARVRCameraOrPancakeCameraTransform gss
                                                                  diffTransform <- Api.godot_transform_operator_multiply povTransform prevTransformInverse
                                                                  return diffTransform
                         (Just (GrabWorkspaces prevTransform)) -> do prevTransformInverse <- Api.godot_transform_affine_inverse prevTransform
                                                                     povTransform <- getARVRCameraOrPancakeCameraTransform gss
                                                                     diffTransform <- Api.godot_transform_operator_multiply povTransform prevTransformInverse
                                                                     return diffTransform
  return diffTransform

validateObject :: GodotObject :< a => a -> Maybe a
validateObject obj = guard (unsafeCoerce ((safeCast obj) :: GodotObject) /= nullPtr) >> return obj

class (GodotObject :< surface) => Validatable surface where
  isValid :: surface -> IO Bool

instance Validatable GodotWlrSurface where
  isValid surf = G.is_valid surf

instance Validatable GodotWlrSubsurface where
  isValid surf = G.is_valid surf

instance Validatable GodotWlrXWaylandSurface where
  isValid surf = G.is_valid surf

instance Validatable GodotWlrXdgSurface where
  isValid surf = G.is_valid surf

instance Validatable GodotWlrXdgToplevel where
  isValid surf = G.is_valid surf

makeIdentityTransform :: IO GodotTransform
makeIdentityTransform = do
  let idTransform = TF (identity :: M33 Float) (V3 0 0 0)
  toLowLevel idTransform

initializeDiffMap :: GodotSpatial -> Vector GodotSpatial -> IO DiffMap
initializeDiffMap gssSpatial workspaces = do
  let workspacesLst = [gssSpatial] ++ (V.toList workspaces)
  transformLst <- Control.Monad.replicateM 11 makeIdentityTransform -- 10 workspaces + 1 gss parent node
  let diffLst = Data.List.zip workspacesLst transformLst
  let diffMap = M.fromList diffLst
  return diffMap

updateDiffMap :: GodotSimulaServer -> GodotSpatial -> GodotTransform -> IO ()
updateDiffMap gss workspaceOrGss newDiff = do
  diffMap <- readTVarIO (gss ^. gssDiffMap)
  let updatedDiffMap = M.insert workspaceOrGss newDiff diffMap
  atomically $ writeTVar (gss ^. gssDiffMap) updatedDiffMap

data NullPointerException = NullPointerException deriving (Eq, Show, Typeable)
instance Exception NullPointerException

validateSurfaceE :: (Validatable surface) => surface -> IO surface
validateSurfaceE surf = do
  case (validateObject surf) of
    Nothing -> (throw NullPointerException)
    Just obj -> do isValidSurface <- isValid surf
                   ret <- if isValidSurface then (return surf) else (Control.Exception.Safe.throw NullPointerException)
                   return ret

catchGodot :: (a -> [GodotVariant] -> IO ()) -> ((a -> [GodotVariant] -> IO ()))
catchGodot func x y = catch (func x y) (\e -> do putStrLn $ "Caught " ++ (show (e :: NullPointerException))
                                                 return ())

data RotationMethod = Workspace | Workspaces

rotateWorkspaceHorizontally :: GodotSimulaServer -> Float -> RotationMethod -> IO ()
rotateWorkspaceHorizontally gss radians rotationMethod = do
  -- get state
  prevPovTransform <- getARVRCameraOrPancakeCameraTransform gss
  povTransform <- getARVRCameraOrPancakeCameraTransform gss
  (currentWorkspace, currentWorkspaceStr) <- readTVarIO (gss ^. gssWorkspace)
  currentWorkspaceTransform <- G.get_transform currentWorkspace

  -- compute new transform
  rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
  case rotationMethod of
      Workspace -> do
        G.rotate currentWorkspace rotationAxisY radians
        currentWorkspaceTransform <- G.get_transform currentWorkspace
        updateDiffMap gss currentWorkspace currentWorkspaceTransform
      Workspaces -> do
        G.rotate gss rotationAxisY radians
        updateDiffMap gss (safeCast gss) currentWorkspaceTransform

  -- update new diff map
  return ()

queueFreeNodeAndChildren :: GodotNode -> IO ()
queueFreeNodeAndChildren node = do
  arrayOfChildren <- G.get_children node :: IO GodotArray
  numChildren <- Api.godot_array_size arrayOfChildren
  arrayOfChildrenGV <- fromGodotArray arrayOfChildren :: IO [GodotVariant]
  childrenNodesLst <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotNode]
  mapM_ G.queue_free childrenNodesLst -- Using G.free instead of G.queue_free just causes a crash
  Api.godot_array_destroy arrayOfChildren
  mapM_ Api.godot_variant_destroy arrayOfChildrenGV
  -- G.queue_free node -- Doens't work

getARVRCameraOrPancakeCamera :: GodotSimulaServer -> IO GodotCamera
getARVRCameraOrPancakeCamera gss = do
  let nodePathStr = "/root/Root/VRViewport/ARVROrigin/ARVRCamera"
  nodePath <- (toLowLevel (pack nodePathStr)) :: IO GodotNodePath
  hasNode  <- G.has_node gss (nodePath :: GodotNodePath)
  camera    <- case hasNode of
        False -> do camera <- getViewportCamera gss
                    return camera
        True ->  do gssNode  <- G.get_node ((safeCast gss) :: GodotNode) nodePath
                    let arvrCamera = (coerce gssNode) :: GodotCamera -- HACK: We use `coerce` instead of something more proper
                    return arvrCamera
  Api.godot_node_path_destroy nodePath
  return camera
  where getViewportCamera :: GodotSimulaServer -> IO GodotCamera
        getViewportCamera gss = do
          viewport <- G.get_viewport gss :: IO GodotViewport
          camera <- G.get_camera viewport :: IO GodotCamera
          return camera

rotateFPSCamera :: GodotSimulaServer -> GodotInputEventMouseMotion -> IO ()
rotateFPSCamera gss event = do
  camera <- getARVRCameraOrPancakeCamera gss
  let mouseSensitivity = 0.006
  eventRelative <- G.get_relative event
  V2 eventRelativeX eventRelativeY <- fromLowLevel eventRelative
  V3 rotationX rotationY rotationZ <- G.get_rotation camera >>= fromLowLevel
  let newRotationY = rotationY - (eventRelativeX * mouseSensitivity)
  let newRotationX' = rotationX - (-eventRelativeY * mouseSensitivity)
  let newRotationX = case ((newRotationX' < -1.5708), (newRotationX' > 1.5708)) of
                          (True, _) -> -1.5708
                          (_, True) -> 1.5708
                          _ -> newRotationX'
  G.set_rotation camera =<< toLowLevel (V3 (newRotationX) (newRotationY) (rotationZ))

processWASDMovement :: GodotSimulaServer -> Float -> IO ()
processWASDMovement gss delta = do
  camera <- getARVRCameraOrPancakeCamera gss
  input <- getSingleton GodotInput "Input"
  moveForward <- toLowLevel "move_forward"
  moveBackward <- toLowLevel "move_backward"
  moveLeft <- toLowLevel "move_left"
  moveRight <- toLowLevel "move_right"
  moveUp <- toLowLevel "move_up"
  moveDown <- toLowLevel "move_down"
  isMoveForward <- G.is_action_pressed input moveForward
  isMoveBackward <- G.is_action_pressed input moveBackward
  isMoveLeft <- G.is_action_pressed input moveLeft
  isMoveRight <- G.is_action_pressed input moveRight
  isMoveUp <- G.is_action_pressed input moveUp
  isMoveDown <- G.is_action_pressed input moveDown
  let motionZ = case (isMoveForward, isMoveBackward) of
                     (True, _) -> -1
                     (_, True) -> 1
                     _ -> 0
  let motionX = case (isMoveLeft, isMoveRight) of
                     (True, _) -> -1
                     (_, True) -> 1
                     _ -> 0
  let motionY = case (isMoveUp, isMoveDown) of
                     (True, _) -> 1
                     (_, True) -> -1
                     _ -> 0
  motion <- Api.godot_vector3_normalized =<< (toLowLevel $ V3 motionX motionY motionZ)
  rotation@(V3 rotationX rotationY rotationZ) <- G.get_rotation camera >>= fromLowLevel
  initialRotation <- readTVarIO (gss ^. gssWasdInitialRotation)
  yVector <- toLowLevel (V3 0 1 0)
  xVector <- toLowLevel (V3 1 0 0)
  zVector <- toLowLevel (V3 0 0 1)
  motion1 <- Api.godot_vector3_rotated motion yVector (realToFrac (rotationY - initialRotation))
  motion2 <- Api.godot_vector3_rotated motion1 xVector (realToFrac ((cos rotationY) * rotationX))
  motion3 <- Api.godot_vector3_rotated motion2 zVector (realToFrac ((-sin rotationY) * rotationX))

  (V3 motionXFinal motionYFinal motionZFinal) <- fromLowLevel motion3
  translationOld <- G.get_translation camera >>= fromLowLevel
  let translationSensitivity = 4
  translationFinal <- toLowLevel $ translationOld + V3 (motionXFinal * translationSensitivity * delta) (motionYFinal * translationSensitivity * delta) (motionZFinal * translationSensitivity * delta)
  G.set_translation camera translationFinal

  Api.godot_string_destroy moveForward
  Api.godot_string_destroy moveBackward
  Api.godot_string_destroy moveLeft
  Api.godot_string_destroy moveRight
  Api.godot_string_destroy moveUp
  Api.godot_string_destroy moveDown

actionPress :: GodotSimulaServer -> String -> IO ()
actionPress gss str = do
  input <- getSingleton GodotInput "Input"
  godotStr <- toLowLevel (pack str)
  G.action_press input godotStr 1.0
  Api.godot_string_destroy godotStr

actionRelease :: GodotSimulaServer -> String -> IO ()
actionRelease gss str = do
  input <- getSingleton GodotInput "Input"
  godotStr <- toLowLevel (pack str)
  G.action_release input godotStr
  Api.godot_string_destroy godotStr

logMemPid :: GodotSimulaServer -> IO Float
logMemPid gss = do
  let pid = (gss ^. gssPid)
  (_, out', _) <- System.Process.readCreateProcessWithExitCode (shell $ "ps -p " ++ pid ++ " -o rss=") ""
  let pidMem = Prelude.read $ out' :: Float
  -- logStr $ "PID mem: " ++ (show pidMem)
  -- Control.Concurrent.threadDelay (1 * 1000000)
  -- logMemPid gss
  return (pidMem / 1000) -- return ~MB

forkUpdateHUDRecursively :: GodotSimulaServer -> IO ()
forkUpdateHUDRecursively gss = do
  configuration <- readTVarIO (gss ^. gssConfiguration)
  let configPath = (configuration ^. hudConfig)
  res@(inp,out,err,pid) <- System.IO.Streams.Process.runInteractiveProcess "./result/bin/i3status" ["-c", configPath] Nothing Nothing --

  forkIO $ updateHUDRecursively gss res
  return ()
  where updateHUDRecursively :: GodotSimulaServer -> (OutputStream B.ByteString, InputStream B.ByteString, InputStream B.ByteString, ProcessHandle) -> IO ()
        updateHUDRecursively gss res@(inp,out,err,pid) = do
          -- let sec = 5
          -- threadDelay (1000 * 1000 * (sec))
          updateWorkspaceHUD gss
          updatei3StatusHUD gss res

          updateHUDRecursively gss res

updateWorkspaceHUD :: GodotSimulaServer -> IO ()
updateWorkspaceHUD gss = do
  (currentWorkspace, currentWorkspaceStr) <- readTVarIO (gss ^. gssWorkspace)
  hud <- readTVarIO (gss ^. gssHUD)
  let canvasLayer = (hud ^. hudCanvasLayer)
  let rtLabelW = (hud ^. hudRtlWorkspace)
  let svr = (hud ^. hudSvrTexture)
  let dynamicFont = (hud ^. hudDynamicFont)
  fps <- getSingleton Godot_Engine "Engine" >>= (\engine -> G.get_frames_per_second engine)
  simulaMemoryUsage <- logMemPid gss
  screenRecorder <- readTVarIO (gss ^. gssScreenRecorder)

  G.clear rtLabelW
  G.push_font rtLabelW (safeCast dynamicFont)
  G.append_bbcode rtLabelW `withGodotString` (pack currentWorkspaceStr)
  G.append_bbcode rtLabelW `withGodotString` " | "
  G.add_image rtLabelW svr 19 19
  G.append_bbcode rtLabelW `withGodotString` " "
  G.append_bbcode rtLabelW `withGodotString` (pack $ (show $ round fps) ++ " FPS ")
  G.append_bbcode rtLabelW `withGodotString` (pack ("@ " ++ (show $ round simulaMemoryUsage) ++ " MB"))
  G.append_bbcode rtLabelW `withGodotString` " |"
  case screenRecorder of
    Nothing -> return ()
    Just ph -> do
      redColor <- (toLowLevel $ (rgb 1.0 0.0 0.0) `withOpacity` 1.0) :: IO GodotColor
      G.push_color rtLabelW redColor
      G.append_bbcode rtLabelW `withGodotString` " ⚬ "
      G.pop rtLabelW
      return ()
  G.pop rtLabelW

updatei3StatusHUD :: GodotSimulaServer -> (OutputStream B.ByteString, InputStream B.ByteString, InputStream B.ByteString, ProcessHandle) -> IO ()
updatei3StatusHUD gss res@(inp,out,err,pid) = do
  hud <- readTVarIO (gss ^. gssHUD)
  out' <- System.IO.Streams.Text.decodeUtf8 out
  maybeLine <- System.IO.Streams.Internal.read out' :: IO (Maybe Text)
  let line = Data.Maybe.fromJust maybeLine
  let line' = unpack line
  let hudNew = hud { _hudI3Status = line' }
  atomically $ writeTVar (gss ^. gssHUD) hudNew
  return ()

withGodotString :: (GodotString -> IO a) -> Text -> IO a
withGodotString action text = do
  godotStr <- toLowLevel text
  ret <- action godotStr
  Api.godot_string_destroy godotStr
  return ret