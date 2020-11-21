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
import           Control.Exception
import           System.Directory

import           Data.Typeable
import           Godot.Gdnative.Types
import           Godot.Nativescript

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
import           Data.Text

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

instance Show GodotWlrXWaylandSurface where
  show wlrXWaylandSurface = (show (coerce wlrXWaylandSurface :: Ptr ()))
instance Show GodotWlrXdgSurface where
  show wlrXWaylandSurface = (show (coerce wlrXWaylandSurface :: Ptr ()))
instance Show GodotWlrSurface where
  show wlrSurface = (show (coerce wlrSurface :: Ptr ()))

instance Ord GodotWlrXWaylandSurface where
  wlrXWaylandSurface1 `compare` wlrXWaylandSurface2 = ((coerce wlrXWaylandSurface1) :: Ptr ()) `compare` ((coerce wlrXWaylandSurface2) :: Ptr ())

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = f b >>= \case
  Just (a, b') -> return . (a :) =<< unfoldrM f b'
  _            -> return []

data SurfaceLocalCoordinates    = SurfaceLocalCoordinates (Float, Float)
data SubSurfaceLocalCoordinates = SubSurfaceLocalCoordinates (Float, Float)
data SpriteDimensions      = SpriteDimensions (Int, Int)

data ResizeMethod = Zoom | Horizontal | Vertical

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
, _keyBindings :: [KeyboardShortcut]
, _keyRemappings :: [KeyboardRemapping]
, _environmentsDirectory :: String
, _environmentDefault :: String
, _defaultTransparency :: Double
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
  , _gssStartingAppsCounter   :: TVar (StartingAppsLaunched, StartingAppsRemaining)
  , _gssStartingApps          :: TVar [String]
  , _gssWorldEnvironment      :: TVar (GodotWorldEnvironment, String)
  , _gssEnvironmentTextures   :: TVar [String]
  , _gssStartingAppTransform  :: TVar (Maybe GodotTransform)
  , _gssPid                   :: String
  }

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = GodotSpatial obj

type SurfaceMap = OMap GodotWlrSurface CanvasSurface

data GodotSimulaViewSprite = GodotSimulaViewSprite
  { _gsvsObj               :: GodotObject
  , _gsvsServer            :: TVar GodotSimulaServer
  , _gsvsShouldMove        :: TVar (Bool, Int)
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
  }

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = GodotRigidBody obj

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
makeLenses ''SimulaView
makeLenses ''GodotSimulaServer
makeLenses ''GodotPancakeCamera
makeLenses ''KeyboardShortcut
makeLenses ''KeyboardRemapping
makeLenses ''StartingApps
makeLenses ''Configuration

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

-- | Unsafe helper function to determine if a Godot type is null.
isGodotTypeNull :: (Typeable a) => a -> IO ()
isGodotTypeNull godotValue = do
  let isNull = ((unsafeCoerce godotValue) == nullPtr)
  putStrLn $ (show (typeOf godotValue)) ++ ": isNull " ++ (show isNull)

isGodotTypeNullErr :: (Typeable a) => a -> IO ()
isGodotTypeNullErr godotValue = do
  let isNull = ((unsafeCoerce godotValue) == nullPtr)
  if isNull then error $ (show (typeOf godotValue)) ++ ": isNull True"
            else putStrLn $ (show (typeOf godotValue)) ++ ": isNull False"

deriving instance Eq GodotWlrOutput
deriving instance Eq GodotWlrXdgSurface
deriving instance Eq GodotWlrXWaylandSurface

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
  if b
    then G.get_singleton engine name' >>= asClass' constr name
    else error $ "No singleton named " ++ (Data.Text.unpack name)

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
    (Left wlrXdgSurface) -> G.get_wlr_surface wlrXdgSurface
    (Right wlrXWaylandSurface) -> G.get_wlr_surface wlrXWaylandSurface


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
  appendFile "log.txt" $ string ++ "\n"

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

  G.set_clear_mode renderTarget G.CLEAR_MODE_ALWAYS
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

savePng :: CanvasSurface -> GodotViewportTexture -> GodotWlrSurface -> IO ()
savePng cs surfaceTexture wlrSurface = do
  let isNull = ((unsafeCoerce surfaceTexture) == nullPtr) || ((unsafeCoerce wlrSurface) == nullPtr)
  case isNull of
    True -> putStrLn "Texture is null in savePng!"
    False -> do -- Get image
                gsvs <- readTVarIO (cs ^. csGSVS)
                surfaceTextureAsImage <- G.get_data surfaceTexture

                -- Get file path
                frame <- readTVarIO (gsvs ^. gsvsFrameCount)
                let pathStr = "./png/" ++ (show (coerce wlrSurface :: Ptr GodotWlrSurface)) ++ "." ++ (show frame) ++ ".png"
                pathStr' <- toLowLevel (pack pathStr)

                -- Save as png
                G.save_png surfaceTextureAsImage pathStr'
                return ()
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
  let relativePath = ("./png/" <> screenshotBaseName <> ".png")
  fullPath <- System.Directory.canonicalizePath relativePath
  G.save_png pancakeImg =<< toLowLevel (pack relativePath)
  return fullPath

-- Run shell command with DISPLAY set to our XWayland server value (typically
-- :2)
appLaunch :: GodotSimulaServer -> String -> [String] -> IO ()
appLaunch gss appStr args = do
  -- logStr $ appStr ++ (show args)

  -- We shouldn't need to set WAYLAND_DISPLAY, but do need to set Xwayland DISPLAY
  let originalEnv = (gss ^. gssOriginalEnv)
  maybeXwaylandDisplay <- readTVarIO (gss ^. gssXWaylandDisplay)
  case (appStr, maybeXwaylandDisplay) of
    ("nullApp", _) -> do appCounter@(appsLaunched, appsRemaining) <- readTVarIO (gss ^. gssStartingAppsCounter)
                         atomically $ writeTVar (_gssStartingAppsCounter gss) (appsLaunched + 1, appsRemaining - 1)
                         sApps <- readTVarIO (gss ^. gssStartingApps)


                         let nextApp = if (sApps == []) then Nothing else Just (Data.List.head sApps)
                         case nextApp of
                           Nothing -> return ()
                           Just app -> do let tailApps = (Data.List.tail sApps)
                                          atomically $ writeTVar (gss ^. gssStartingApps) tailApps
                                          let secondApp = if tailApps == [] then Nothing else Just (Data.List.head tailApps)
                                          case secondApp of
                                            Nothing -> return ()
                                            Just secondApp' -> appStrLaunch gss secondApp'

                         return ()
    ("launchHMDWebcam", _) -> launchHMDWebCam gss
    ("launchTerminal", _) -> terminalLaunch gss
    ("launchUsageInstructions", _) -> appStrLaunch gss "./result/bin/midori https://github.com/SimulaVR/Simula#usage -p"
    (_, Nothing) -> putStrLn "No DISPLAY found!"
    (_, (Just xwaylandDisplay)) -> do
      let envMap = M.fromList originalEnv
      let envMapWithDisplay = M.insert "DISPLAY" xwaylandDisplay envMap
      let envMapWithWaylandDisplay = M.insert "WAYLAND_DISPLAY" "simula-0" envMapWithDisplay
      -- let envMapWithDisplay = M.insert "DISPLAY" ":13" envMap
      let envListWithDisplays = M.toList envMapWithWaylandDisplay
      res <- try $ createProcess (proc appStr args) { env = Just envListWithDisplays, new_session = True, std_out = NoStream, std_err = NoStream } :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
      -- res <- try $ createProcess (proc appStr args) { env = Just envListWithDisplay, new_session = True } :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
      case res of
        Left _ -> putStrLn $ "Cannot find command: " ++ appStr
        Right _ -> return ()
  return ()

launchHMDWebCam :: GodotSimulaServer -> IO ()
launchHMDWebCam gss = do
  maybePath <- getHMDWebCamPath
  case maybePath of
    Nothing -> do putStrLn "Cannot find HMD web cam!"
                  appStrLaunch gss "nullApp"
    Just path  -> appLaunch gss "./result/bin/ffplay" ["-loglevel", "quiet", "-f", "v4l2", path]
    where getHMDWebCamPath :: IO (Maybe FilePath)
          getHMDWebCamPath = do
            res <- try $ listDirectory "/dev/v4l/by-id" :: IO (Either IOException [FilePath])
            case res of
              Left _ -> return Nothing
              Right _ -> (listToMaybe . Data.List.map ("/dev/v4l/by-id/" ++) . sort . Data.List.filter viveOrValve) <$> listDirectory "/dev/v4l/by-id"
          viveOrValve :: String -> Bool
          viveOrValve str = Data.List.any (`Data.List.isInfixOf` str) ["Vive",  -- HTC Vive
                                                                       "VIVE",  -- HTC Vive Pro
                                                                       "Valve", -- Valve Index?
                                                                       "Etron"] -- Valve Index

terminalLaunch :: GodotSimulaServer -> IO ()
terminalLaunch gss = appLaunch gss "./result/bin/xfce4-terminal" []

appStrLaunch :: GodotSimulaServer -> String -> IO ()
appStrLaunch gss appStr = do
  let rootCmd = Data.List.head (Data.List.words appStr)
  let args = Data.List.tail (Data.List.words appStr)
  appLaunch gss rootCmd args

getTextureFromURL :: String -> IO (Maybe GodotTexture)
getTextureFromURL urlStr = do
   godotImage <- unsafeInstance GodotImage "Image" :: IO GodotImage
   godotImageTexture <- unsafeInstance GodotImageTexture "ImageTexture"
   pngUrl <- toLowLevel (pack urlStr) :: IO GodotString
   exitCode <- G.load godotImage pngUrl
   -- G.compress godotImage G.COMPRESS_ETC2 G.COMPRESS_SOURCE_GENERIC 1
   G.create_from_image godotImageTexture godotImage G.TEXTURE_FLAGS_DEFAULT
   if (unsafeCoerce godotImageTexture == nullPtr) then (return Nothing) else (return (Just (safeCast godotImageTexture)))

loadEnvironmentTextures :: Configuration -> GodotWorldEnvironment -> IO [String]
loadEnvironmentTextures configuration worldEnvironment = do
  -- configuration <- readTVarIO (gss ^. gssConfiguration)
  let envDir = (configuration ^. environmentsDirectory)
  dirExists <- try $ listDirectory envDir :: IO (Either IOException [FilePath])
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
                               -- G.unreference @GodotReference (safeCast oldTex) -- Doesn't actually fix leak
                               -- Api.godot_object_destroy $ safeCast oldTex -- Causes crash, which means oldTex is still being used somehow
                               return ()
  where next :: Eq a => Maybe a -> [a] -> Maybe a
        next _ []             = Nothing
        next Nothing    (x:_) = Just x
        next (Just e) l@(x:_) = Just $ case Data.List.dropWhile (/= e) l of
                                  (_:y:_) -> y
                                  _       -> x

orientSpriteTowardsGaze :: GodotSimulaViewSprite -> IO ()
orientSpriteTowardsGaze gsvs = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  isInSceneGraph <- G.is_a_parent_of ((safeCast gss) :: GodotNode ) ((safeCast gsvs) :: GodotNode)
  case isInSceneGraph of
    False -> putStrLn "Nothing to orient!"
    True -> do gss <- readTVarIO (gsvs ^. gsvsServer)
               upV3 <- toLowLevel (V3 0 1 0) :: IO GodotVector3
               rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
               targetV3 <- getARVRCameraOrPancakeCameraTransform gss >>= Api.godot_transform_get_origin -- void look_at ( Vector3 target, Vector3 up )
               G.look_at gsvs targetV3 upV3                      -- The negative z-axis of the gsvs looks at HMD
               return ()
               -- G.rotate_object_local gsvs rotationAxisY 3.14159  -- The positive z-axis of the gsvs looks at HMD

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
                True -> do orientSpriteTowardsGaze gsvs
                           return $ SpriteDimensions (round $ ((fromIntegral w) * factor), round $ ((fromIntegral h)))
                False -> return $ oldTargetDims
            Vertical   -> do
              case (((fromIntegral h) * factor) > 500) of
                True -> do orientSpriteTowardsGaze gsvs
                           return $ SpriteDimensions (round $ ((fromIntegral w)), round $ ((fromIntegral h) * factor))
                False -> return $ oldTargetDims
            Zoom       -> do
              case (((fromIntegral h) * factor) > 500) && (((fromIntegral h) * factor)  < 1500) of
                False -> return $ oldTargetDims
                True -> do V3 1 1 1 ^* (1 + 1 * (1 - factor)) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)
                           return $ SpriteDimensions (round $ ((fromIntegral w) * factor), round $ ((fromIntegral h) * factor))

     atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just newTargetDims)

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

    atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just newTargetDims)

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
                let pathStr = "./png/" ++  ((Data.List.filter (/= '"') . show) timeStampStr) ++ ".png"
                pathStr' <- toLowLevel (pack pathStr)

                -- Save as png
                rect <- toLowLevel m22
                rectImage <- G.get_rect texAsImage rect
                System.Directory.createDirectoryIfMissing True "png"
                G.save_png rectImage pathStr'
                gss <- readTVarIO (gsvs ^. gsvsServer)

                -- Copy to clipboard
                appLaunch gss "./result/bin/xclip" ["-selection", "clipboard", "-t", "image/png", "-i", pathStr]

                return ()

fromGodotArray :: GodotArray -> IO [GodotVariant]
fromGodotArray vs = do
  size <- fromIntegral <$> Api.godot_array_size vs
  forM [0..size-1] $ Api.godot_array_get vs