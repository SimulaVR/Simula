{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Util where

import           Data.Function           ((&))
import           Data.Text               as T

import           Foreign.C               (withCString)

import           Godot.Api
import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types
import qualified Godot.Methods as G
import           Godot.Internal.Dispatch

import           Plugin.Util.Types


godotPrint :: Text -> IO ()
godotPrint str = godot_print =<< toLowLevel str


safeInstance :: (GodotObject :< a) => (GodotObject -> a) -> Text -> IO (Maybe a)
safeInstance constr className = do
  classDB <- getClassDB
  vt <- (G.instance' classDB =<< toLowLevel className) >>= fromLowLevel
  case fromVariant vt :: Maybe GodotObject of
    Just obj -> asClass constr className obj
    Nothing -> return Nothing

classInstance :: (GodotObject :< a) => (GodotObject -> a) -> Text -> IO a
classInstance constr className =
  safeInstance constr className >>= \case
    Just a -> return a
    Nothing -> error $ unpack $ "Could not instance " `append` className

nsInstance :: (GodotObject :< a)
  => (GodotObject -> a) -> Text -> [Variant 'GodotTy] -> Text -> IO a
nsInstance constr clsName args url = do
  loadResource GodotNativeScript "NativeScript" url
    >>= (flip G.new args :: GodotNativeScript -> IO GodotObject)
    >>= asClass' constr clsName

sceneInstance :: (GodotNode :< a)
  => Int -> (GodotObject -> a) -> Text -> Text -> IO a
sceneInstance genEditState constr clsName url =
  loadResource GodotPackedScene "PackedScene" url
    >>= flip G.instance' genEditState
    >>= asClass' constr clsName

loadResource :: (GodotResource :< a) => (GodotObject -> a) -> Text -> Text -> IO a
loadResource constr clsName url = do
  rl <- getResourceLoader
  url' <- toLowLevel url
  clsName' <- toLowLevel clsName
  res <- G.load rl url' clsName' False >>= asClass constr clsName
  case res of
    Just a -> return a
    Nothing -> error $ unpack $ T.unwords ["Could not instantiate ", url, " as a ", clsName]

worldScale :: IO Float
worldScale = do
  arvrServer <- GodotARVRServer <$> getSingleton "ARVRServer"
  G.get_world_scale arvrServer

reparentSpatial :: GodotSpatial -> GodotNode -> IO ()
reparentSpatial node newParent = do
  tf <- G.get_global_transform node
  parent <- G.get_parent node
  G.remove_child parent (safeCast node)
  G.add_child newParent (safeCast node) True
  G.set_global_transform node tf

reparent :: GodotNode -> GodotNode -> IO ()
reparent node newParent = do
  isSpatial <- G.is_class node =<< toLowLevel "Spatial"
  if isSpatial
    then reparentSpatial (GodotSpatial (safeCast node)) newParent
    else do
      parent <- G.get_parent node
      G.remove_child parent (safeCast node)
      G.add_child newParent (safeCast node) True

getNode :: (GodotNode :< a) => a -> NodePath -> IO (Maybe GodotNode)
getNode self np = do
  np' <- toLowLevel np :: IO GodotNodePath
  hasNode <- (safeCast self :: GodotNode) `G.has_node` np'
  if hasNode
    then Just <$> G.get_node (safeCast self :: GodotNode) np'
    else return Nothing

getSingleton :: Text -> IO GodotObject
getSingleton name = godot_global_get_singleton
  & withCString (T.unpack name)


getClassDB :: IO Godot_ClassDB
getClassDB = Godot_ClassDB <$> getSingleton "ClassDB"

getVisualServer :: IO GodotVisualServer
getVisualServer = GodotVisualServer <$> getSingleton "VisualServer"

getResourceLoader :: IO Godot_ResourceLoader
getResourceLoader = Godot_ResourceLoader <$> getSingleton "ResourceLoader"

getInput :: IO GodotInput
getInput = GodotInput <$> getSingleton "Input"

clamp :: (Floating a, Ord a) => a -> a -> a -> a
clamp min' max' val = max min' $ min max' val

-- Debug

debugPrint :: Show a => Text -> a -> IO a
debugPrint msg a = do
  godotPrint $ msg `append` (pack $ show a)
  return a
