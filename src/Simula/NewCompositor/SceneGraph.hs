module Simula.NewCompositor.SceneGraph where

import Control.Monad
import Control.Monad.Loops
import Data.IORef
import Data.Typeable
import Linear

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.SceneGraph.Output.Display
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.Types

class (Eq a, Typeable a) => SceneGraphNode a where
  nodeOnFrameBegin :: a -> Maybe Scene -> IO ()
  nodeOnFrameBegin _ _ = return ()
  
  nodeOnFrameDraw :: a -> Maybe Scene -> IO ()
  nodeOnFrameDraw _ _ = return ()
  
  nodeOnFrameEnd :: a -> Maybe Scene -> IO ()
  nodeOnFrameEnd _ _ = return ()
  
  nodeOnWorldTransformChange :: a -> Maybe Scene -> IO ()
  nodeOnWorldTransformChange _ _ = return ()

  nodeParent :: a -> IO (Maybe (Some SceneGraphNode))
  setNodeParent' :: a -> Maybe (Some SceneGraphNode) -> IO ()
  nodeChildren :: a -> IORef [Some SceneGraphNode]
  nodeScene :: a -> IO (Maybe Scene)
  nodeScene this = nodeParent this >>= \case
    Just (Some prt) -> nodeScene prt
    Nothing -> return Nothing

  nodeTransform :: a -> IO (M44 Float)
  setNodeTransform' :: a -> M44 Float -> IO ()
  
  isSurfaceNode :: a -> Bool
  isSurfaceNode _ = False

  {- virtual Geometry::RaySurfaceIntersection *intersectWithSurfaces(const Geometry::Ray &ray); -}
  {- Geometry::RaySurfaceIntersection *SceneGraphNode::intersectWithSurfaces(const Geometry::Ray &ray)
{
    Geometry::RaySurfaceIntersection *closestIntersection = NULL, *currentIntersection;
    Geometry::Ray transformedRay = ray.transform(inverseTransform());
    for (SceneGraphNode *child : m_childNodes) {
        if (child != NULL){
            currentIntersection = child->intersectWithSurfaces(transformedRay);
            if(closestIntersection == NULL || (currentIntersection != NULL && currentIntersection->t < closestIntersection->t)){
                delete closestIntersection;
                closestIntersection = currentIntersection;
            }
        }
    }
    return closestIntersection;
}
 -}

setNodeParent :: SceneGraphNode a => a -> Maybe (Some SceneGraphNode) -> IO ()
setNodeParent this Nothing = setNodeParent' this Nothing
setNodeParent this x@(Just (Some prt)) = case cast prt of
  Just prt' | this == prt' -> setNodeParent' this Nothing
  _ -> setNodeParent' this x >> modifyIORef' (nodeChildren prt) (++ [Some this])

nodeSubtreeContains :: (SceneGraphNode a, SceneGraphNode b) => a -> b -> IO Bool
nodeSubtreeContains this node = case cast node of
  Just node' -> return $ this == node'
  Nothing -> readIORef (nodeChildren this) >>= anyM (\(Some child) -> nodeSubtreeContains child node)


setNodeTransform :: SceneGraphNode a => a -> M44 Float -> IO ()
setNodeTransform this tf = do
  setNodeTransform' this tf
  nodeScene this >>= nodeMapOntoSubtree this (\(Some node) -> nodeOnWorldTransformChange node)

nodeWorldTransform :: SceneGraphNode a => a -> IO (M44 Float)
nodeWorldTransform this = nodeParent this >>= \case
  Just (Some prt) -> liftM2 (!*!) (nodeWorldTransform prt) (nodeTransform this)
  Nothing -> nodeTransform this
  
setNodeWorldTransform :: SceneGraphNode a => a -> M44 Float -> IO ()
setNodeWorldTransform this tf = nodeParent this >>= \case
  Just (Some prt) -> fmap (!*! tf) (inv44 <$> nodeWorldTransform prt) >>= setNodeTransform this
  Nothing -> setNodeTransform this tf


nodeMapOntoSubtree :: SceneGraphNode a => a -> (Some SceneGraphNode -> Maybe Scene -> IO ()) -> Maybe Scene -> IO ()
nodeMapOntoSubtree this func scene = do
  func (Some this) scene
  readIORef (nodeChildren this) >>= mapM_ (\(Some child) -> nodeMapOntoSubtree child func scene)

data BaseSceneGraphNode = GraphNode {
  _graphNodeChildren :: IORef [Some SceneGraphNode],
  _graphNodeParent :: IORef (Maybe (Some SceneGraphNode)),
  _graphNodeTransform :: IORef (M44 Float)
  }
  deriving (Eq, Typeable)

baseNodeParent :: BaseSceneGraphNode -> IO (Maybe (Some SceneGraphNode))
baseNodeParent = readIORef . _graphNodeParent

setBaseNodeParent :: BaseSceneGraphNode -> Maybe (Some SceneGraphNode) -> IO ()
setBaseNodeParent = writeIORef . _graphNodeParent

baseNodeTransform :: BaseSceneGraphNode -> IO (M44 Float)
baseNodeTransform = readIORef . _graphNodeTransform

setBaseNodeTransform :: BaseSceneGraphNode -> M44 Float -> IO ()
setBaseNodeTransform = writeIORef . _graphNodeTransform

data Scene = Scene {
  _sceneBase :: BaseSceneGraphNode,
  _sceneCurrentTimestamp :: IORef Int,
  _sceneLastTimestamp :: IORef Int,
  _sceneWindowManager :: IORef (Some WindowManager),
  _sceneCompositor :: IORef (Some Compositor),
  _sceneTrash :: IORef Scene,
  _sceneDisplays :: IORef [Some Display],
  _sceneActiveDisplay :: IORef (Some Display)
  }
  deriving (Eq, Typeable)

instance SceneGraphNode Scene where
  nodeParent = baseNodeParent . _sceneBase
  setNodeParent' = setBaseNodeParent . _sceneBase
  nodeTransform = baseNodeTransform . _sceneBase
  setNodeTransform' = setBaseNodeTransform . _sceneBase
  nodeChildren = _graphNodeChildren . _sceneBase

setSceneTimestamp :: Scene -> Int -> IO ()
setSceneTimestamp this ts = do
  prev <- readIORef $ _sceneCurrentTimestamp this
  writeIORef (_sceneLastTimestamp this) prev
  writeIORef (_sceneCurrentTimestamp this) ts

scenePrepareForFrame :: Scene -> Int -> IO ()
scenePrepareForFrame this ts = do
  setSceneTimestamp this ts
  nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameBegin node) (Just this)
  dps <- readIORef (_sceneDisplays this)
  forM_ dps $ \(Some dp) -> do
    vps <- readIORef (displayViewpoints dp)
    mapM_ vpUpdateViewMatrix vps
    
sceneDrawFrame :: Scene -> IO ()
sceneDrawFrame this = do
  dps <- readIORef (_sceneDisplays this)
  forM_ dps $ \(Some dp) -> do
    writeIORef (_sceneActiveDisplay this) (Some dp)
    displayPrepareToDraw dp
    nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameDraw node) (Just this)
    displayFinishDraw dp

sceneFinishFrame :: Scene -> IO ()
sceneFinishFrame this = do
  nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameEnd node) (Just this)
  {- int error = glGetError();
    if(error != GL_NO_ERROR){
        std::cout <<  "OpenGL Error from frame: " << error <<std::endl;
    } -}
