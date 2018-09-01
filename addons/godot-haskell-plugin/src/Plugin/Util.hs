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


godotPrint :: Text -> IO ()
godotPrint str = godot_print =<< toLowLevel str


mkClassInstance :: Text -> IO GodotObject
mkClassInstance className = do
  classDB <- getClassDB
  cls <- toLowLevel className
    >>= G.instance' classDB
    >>= fromGodotVariant
  return cls


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

