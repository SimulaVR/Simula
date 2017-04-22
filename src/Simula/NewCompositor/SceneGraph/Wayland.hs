module Simula.NewCompositor.SceneGraph.Wayland where

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Int
import Data.Typeable
import Data.Word
import Linear
import Linear.OpenGL

import Graphics.Rendering.OpenGL hiding (scale)
import Foreign

import Simula.NewCompositor.Utils
import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Types
import Simula.NewCompositor.Wayland.Output

data WaylandSurfaceNode = WaylandSurfaceNode {
  _waylandSurfaceNodeBase :: BaseDrawable,
  _waylandSurfaceNodeSurface :: IORef (Some WaylandSurface),
  _waylandSurfaceNodeSurfaceTransform :: IORef (M44 Float),
  _waylandSurfaceNodeDecorations :: WireframeNode,
  _waylandSurfaceNodeTextureCoords :: BufferObject,
  _waylandSurfaceNodeVertexCoords :: BufferObject,
  _waylandSurfaceNodeShader :: Program,
  _waylandSurfaceNodeAPosition, _waylandSurfaceNodeATexCoord :: AttribLocation,
  _waylandSurfaceNodeUMVPMatrix :: UniformLocation
  } deriving (Eq, Typeable)

instance SceneGraphNode WaylandSurfaceNode where
  nodeParent = baseNodeParent . _baseDrawableBase . _waylandSurfaceNodeBase
  setNodeParent' = setBaseNodeParent . _baseDrawableBase . _waylandSurfaceNodeBase
  nodeTransform = baseNodeTransform . _baseDrawableBase . _waylandSurfaceNodeBase
  setNodeTransform' = setBaseNodeTransform . _baseDrawableBase . _waylandSurfaceNodeBase
  nodeChildren = _graphNodeChildren . _baseDrawableBase . _waylandSurfaceNodeBase

  nodeOnFrameBegin this _ = do
    computeSurfaceTransform this 8
    Some surface <- readIORef $ _waylandSurfaceNodeSurface this
    wsPrepare surface
    
  nodeOnFrameDraw = drawableOnFrameDraw

instance VirtualNode WaylandSurfaceNode

instance Drawable WaylandSurfaceNode where
  drawableVisible = baseDrawableVisible . _waylandSurfaceNodeBase
  setDrawableVisible = setBaseDrawableVisible . _waylandSurfaceNodeBase

  drawableDraw this scene display = do
    Some surface <- readIORef $ _waylandSurfaceNodeSurface this
    texture <- wsTexture surface

    currentProgram $= Just (_waylandSurfaceNodeShader this)

    let aPosition = _waylandSurfaceNodeAPosition this
    let aTexCoord = _waylandSurfaceNodeATexCoord this
    let uMVPMatrix = _waylandSurfaceNodeUMVPMatrix this
    let vertexCoords = _waylandSurfaceNodeVertexCoords this
    let textureCoords = _waylandSurfaceNodeTextureCoords this
    
    vertexAttribArray aPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vertexCoords
    vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)

    vertexAttribArray aTexCoord $= Enabled
    bindBuffer ArrayBuffer $= Just textureCoords
    vertexAttribPointer aTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)

    textureBinding Texture2D $= Just texture
    textureFilter Texture2D $= ((Linear', Nothing), Linear')

    surfaceTf <- readIORef $ _waylandSurfaceNodeSurfaceTransform this
    viewpoints <- readIORef $ _displayViewpoints display
    forM_ viewpoints $ \vp -> do
      --TODO compare w/ order in draw for WireFrameNode 
      port <- readIORef $ _viewPointViewPort vp
      setViewPort port
      
      projMatrix <- readIORef $ _viewPointProjectionMatrix vp
      viewMatrix <- readIORef $ _viewPointViewMatrix vp
      worldTf <- nodeWorldTransform this
    
      let mat = (projMatrix !*! viewMatrix !*! worldTf !*! surfaceTf) ^. m44GLmatrix
      uniform uMVPMatrix $= mat
      drawArrays TriangleFan 0 4

    textureBinding Texture2D $= Nothing
    vertexAttribArray aPosition $= Disabled
    vertexAttribArray aTexCoord $= Disabled
    currentProgram $= Nothing

computeSurfaceTransform :: WaylandSurfaceNode -> Float -> IO ()
computeSurfaceTransform this ppcm = when (ppcm > 0) $ do
  let ppm = 100*ppcm
  let rotQ = axisAngle (V3 0 0 1) pi
  -- TODO test if it's identical
  let rotM = mkTransformation rotQ (V3 (negate 0.5) (negate 0.5) 0)

  Some surface <- readIORef $ _waylandSurfaceNodeSurface this
  size <- (fmap . fmap) fromIntegral $ wsSize surface
  
  let scaleM = scale identity $ V3 (negate (size ^. _x) / ppm)  ((size ^. _y) / ppm) 1

  writeIORef (_waylandSurfaceNodeSurfaceTransform this) $ scaleM !*! rotM

  setNodeTransform (_waylandSurfaceNodeDecorations this) $ scaleM !*! mkTransformation rotQ (V3 0 0 0) !*! scale identity (V3 1.04 1.04 0)

newWaylandSurfaceNode :: (WaylandSurface ws, SceneGraphNode a) => ws -> a -> M44 Float -> IO WaylandSurfaceNode
newWaylandSurfaceNode ws parent tf = do
  program <- getProgram ShaderMotorcarSurface

  texCoords <- genObjectName
  bindBuffer ArrayBuffer $= Just texCoords
  withArrayLen textureCoordinates $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)
  
  verCoords <- genObjectName
  bindBuffer ArrayBuffer $= Just verCoords
  withArrayLen vertexCoordinates $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)

  aPos <- get $ attribLocation program "aPosition"
  aTex <- get $ attribLocation program "aTexCoord"
  uMVP <- get $ uniformLocation program "uMVPMatrix"

  let decoVert = concat [ mkDeco i j k | i <- [-1,1], j <- [-1,1], k <- [-1,1] ]
  let decoColor = Color3 0.5 0.5 0.5

  rec decoNode <- newWireframeNode decoVert decoColor node identity
      node <- WaylandSurfaceNode
              <$> newBaseDrawable (Just (Some parent)) tf
              <*> newIORef (Some ws)
              <*> newIORef identity
              <*> pure decoNode
              <*> pure texCoords
              <*> pure verCoords
              <*> pure program
              <*> pure aPos
              <*> pure aTex
              <*> pure uMVP
  return node

  where
    textureCoordinates :: [Float]
    textureCoordinates = [0, 0, 0, 1, 1, 1, 1, 0]

    vertexCoordinates :: [Float]
    vertexCoordinates = [0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0]

    mkDeco i j k =
      let direction = V3 i j k
          corner = direction ^* 0.5 :: V3 Float
      in concatMap (mkVertices corner direction) [ex, ey, ez]

    mkVertices corner@(V3 cx cy cz) direction (E coord) =
      let V3 sx sy sz = corner & coord -~ (direction ^. coord)
      in [cx, cy, cz, sx, sy, sz]


{-
bool WaylandSurfaceNode::computeLocalSurfaceIntersection(const Geometry::Ray &localRay, glm::vec2 &localIntersection, float &t)
{
    Geometry::Plane surfacePlane = Geometry::Plane(glm::vec3(0), glm::vec3(0.0f,0.0f,1.0f));
    if(glm::dot(localRay.d, surfacePlane.n) == 0) return false;

    Geometry::Ray transformedRay = localRay.transform(glm::inverse(surfaceTransform()));

    //transformedRay.print();

    t = surfacePlane.intersect(transformedRay);
    //std::cout << "t = " << t << std::endl;
    glm::vec3 intersection = transformedRay.solve(t);

    //Geometry::printVector(glm::vec3(intersection));

    //transformedRay.print();
    //transformedRay.draw(this, glm::vec3(0,0,1),  surfaceTransform());

    glm::vec3 coords= intersection * glm::vec3(m_surface->size().x, m_surface->size().y, 0);
    localIntersection =  glm::vec2(coords);

    return t >= 0;
}

Geometry::RaySurfaceIntersection *WaylandSurfaceNode::intersectWithSurfaces(const Geometry::Ray &ray)
{
    Geometry::RaySurfaceIntersection *closestSubtreeIntersection = SceneGraphNode::intersectWithSurfaces(ray);

    if(((int) m_surface->type()) == WaylandSurface::SurfaceType::CURSOR){
        return closestSubtreeIntersection;
    }

    Geometry::Ray localRay = ray.transform(inverseTransform());

    float t;
    glm::vec2 localIntersection;
    bool isIntersected = computeLocalSurfaceIntersection(localRay, localIntersection, t);


    if(isIntersected && (closestSubtreeIntersection == NULL || t < closestSubtreeIntersection-> t)
            && localIntersection.x >= 0 && localIntersection.x <= m_surface->size().x
            && localIntersection.y >= 0 && localIntersection.y <= m_surface->size().y){

            return new Geometry::RaySurfaceIntersection(this, localIntersection, ray, t);


    }else{
        return closestSubtreeIntersection;
    }
}
-}
