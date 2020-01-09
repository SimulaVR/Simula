{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin.Types where

import           Data.Maybe
import           Control.Monad
import           Data.Coerce
import           Unsafe.Coerce

import           Data.Typeable
import           Godot.Gdnative.Types
import           Godot.Nativescript

import           Plugin.Imports

import qualified Godot.Methods               as G
import           Godot.Gdnative.Types -- for Variant access
import           Godot.Gdnative.Internal.Api as Api
import           Godot.Nativescript as NativeScript

import qualified Godot.Core.GodotImage       as Image
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

-- import           Graphics.Wayland.Internal.Server
-- import           Graphics.Wayland.WlRoots.Surface
-- import           Graphics.Wayland.Signal
-- import           Graphics.Wayland.WlRoots.XdgShell
-- import           Graphics.Wayland.WlRoots.Buffer
-- import           Graphics.Wayland.Internal.SpliceServerTypes (Buffer(..))
-- import           Graphics.Wayland.Server
-- import           Graphics.Wayland.Internal.Server
-- import           Graphics.Wayland.Internal.SpliceServerTypes
-- -- import           Graphics.Wayland.WlRoots.Compositor
-- import           Graphics.Wayland.WlRoots.Output
-- import           Graphics.Wayland.WlRoots.Surface
-- import           Graphics.Wayland.WlRoots.Backend
-- import           Graphics.Wayland.Signal
-- import           Graphics.Wayland.WlRoots.Render
-- -- import           Graphics.Wayland.WlRoots.Render.Color
-- -- import           Graphics.Wayland.WlRoots.OutputLayout
-- import           Graphics.Wayland.WlRoots.Input
-- import           Graphics.Wayland.WlRoots.Seat
-- -- import           Graphics.Wayland.WlRoots.Cursor
-- -- import           Graphics.Wayland.WlRoots.XCursorManager
-- import           Graphics.Wayland.WlRoots.XdgShell
-- import           Graphics.Wayland.WlRoots.Input.Keyboard
-- -- import           Graphics.Wayland.WlRoots.Input.Pointer
-- -- import           Graphics.Wayland.WlRoots.Cursor
-- import           Graphics.Wayland.WlRoots.Input.Buttons
-- import           Graphics.Wayland.WlRoots.Box
import qualified Data.Map.Strict as M

import Data.UUID

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = f b >>= \case
  Just (a, b') -> return . (a :) =<< unfoldrM f b'
  _            -> return []

data SurfaceLocalCoordinates    = SurfaceLocalCoordinates (Float, Float)
data SubSurfaceLocalCoordinates = SubSurfaceLocalCoordinates (Float, Float)

-- This should ideally be `[Variant 'HaskellTy]`, but that would
-- require `AsVariant` to handle both `LibType`s.
type instance TypeOf 'HaskellTy GodotArray = [GodotVariant]
instance GodotFFI GodotArray [GodotVariant] where
  fromLowLevel vs = do
    size <- fromIntegral <$> Api.godot_array_size vs
    let maybeNext n v =
          if n == (size - 1)
          then Nothing
          else Just (v, n + 1)
    let variantAt n =
          maybeNext n <$> (Api.godot_array_get vs n)
    unfoldrM variantAt 0

  toLowLevel vs = do
    array <- Api.godot_array_new
    mapM_ (Api.godot_array_append array) vs
    return array

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
  , _gssHMDRayCast            :: TVar (GodotRayCast)
  , _gssKeyboardGrabbedSprite :: TVar (Maybe (GodotSimulaViewSprite, Float)) -- We encode both the gsvs and its original distance from the user
  , _gssXWaylandDisplay       :: TVar (Maybe String) -- For appLaunch
  }

instance HasBaseClass GodotSimulaServer where
  type BaseClass GodotSimulaServer = GodotSpatial
  super (GodotSimulaServer obj _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = GodotSpatial obj

-- Wish there was a more elegant way to jam values into these fields at classInit
data GodotSimulaViewSprite = GodotSimulaViewSprite
  { _gsvsObj               :: GodotObject
  , _gsvsServer            :: TVar GodotSimulaServer    -- Contains the WlrSeat
  , _gsvsShouldMove        :: TVar Bool
  , _gsvsSprite            :: TVar GodotSprite3D
  , _gsvsShape             :: TVar GodotBoxShape
  , _gsvsView              :: TVar SimulaView -- Contains Wlr data
  , _gsvsSimulaCanvasItem  :: TVar GodotSimulaCanvasItem
  , _gsvsCursorCoordinates :: TVar SurfaceLocalCoordinates
  -- , _gsvsMapped         :: TVar Bool
  -- , gsvsGeometry        :: GodotRect2
  -- , gsvsWlrSeat         :: GodotWlrSeat
  -- , gsvsInputMode       :: TVar InteractiveMode
  }

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ _ _ _) = GodotRigidBody obj


data GodotSimulaCanvasItem = GodotSimulaCanvasItem {
     _gsciObject :: GodotObject -- Meant to extend/be casted as GodotCanvasItem
   , _gsciGSVS :: TVar GodotSimulaViewSprite
   , _gsciViewport :: TVar GodotViewport
 }

instance HasBaseClass GodotSimulaCanvasItem where
  type BaseClass GodotSimulaCanvasItem = GodotNode2D
  super (GodotSimulaCanvasItem obj _ _ ) = GodotNode2D obj

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
makeLenses ''GodotSimulaCanvasItem
makeLenses ''SimulaView
makeLenses ''GodotSimulaServer
makeLenses ''GodotPancakeCamera

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
  objClass <- G.get_class (GodotNode $ safeCast obj) >>= fromLowLevel
  let clsName' =
        if objClass /= "" && Data.Text.head objClass == '_'
        then "_" `Data.Text.append` clsName
        else clsName
  ((toLowLevel clsName') :: IO GodotString) >>= G.is_class ((safeCast obj) :: GodotObject)

-- Leaks
getEngine :: IO Godot_Engine
getEngine = Api.godot_global_get_singleton & withCString (unpack "Engine")
  >>= asClass' Godot_Engine "Engine"

godotPrint :: Text -> IO ()
godotPrint str = Api.godot_print =<< toLowLevel str

printGSVS :: GodotSimulaViewSprite -> IO ()
printGSVS gsvs = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let maybeID = (simulaView ^. gsvsUUID)
  case maybeID of
     Nothing -> putStrLn "Couldn't get GSVS ID"
     (Just id) -> putStrLn $ "gsvs id: " ++ (show id)

getWlrSurface :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface -> IO GodotWlrSurface
getWlrSurface eitherSurface = do
  case eitherSurface of
    (Left wlrXdgSurface) -> G.get_wlr_surface wlrXdgSurface
    (Right wlrXWaylandSurface) -> G.get_wlr_surface wlrXWaylandSurface


-- For reference: this is the buggy instance imported from godot-extra that we replace.
-- type instance TypeOf 'HaskellTy GodotArray = [GodotVariant]
-- instance GodotFFI GodotArray [GodotVariant] where
--   fromLowLevel vs = do
--     size <- fromIntegral <$> Api.godot_array_size vs
--     let maybeNext n v =
--           if n == (size - 1)
--           then Nothing
--           else Just (v, n + 1)
--     let variantAt n =
--           maybeNext n <$> (Api.godot_array_get vs n)
--     unfoldrM variantAt 0

--   toLowLevel vs = do
--     array <- Api.godot_array_new
--     mapM_ (Api.godot_array_append array) vs
--     return array

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
  sprite <- atomically $ readTVar (_gsvsSprite gsvs)
  aabb <- G.get_aabb sprite
  size <- godot_aabb_get_size aabb >>= fromLowLevel
  let topleftPos =
        V2 (size ^. _x / 2 - lpos ^. _x) (size ^. _y / 2 - lpos ^. _y)
  let scaledPos = liftI2 (/) topleftPos (size ^. _xy)
  rect <- G.get_item_rect sprite
  recSize <- godot_rect2_get_size rect >>= fromLowLevel
  let coords = liftI2 (*) recSize scaledPos
  -- coords = surface coordinates in pixel with (0,0) at top left
  let sx = fromIntegral $ truncate (1 * coords ^. _x) -- 256 was old factor
      sy = fromIntegral $ truncate (1 * coords ^. _y) -- 256 was old factor
  clickPos' <- fromLowLevel clickPos
  -- putStrLn $ "getSurfaceLocalCoordinates clickPos: " ++ (show clickPos')
  -- putStrLn $ "getSurfaceLocalCoordinates (sx, sy):" ++ "(" ++ (show sx) ++ ", " ++ (show sy) ++ ")"
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
