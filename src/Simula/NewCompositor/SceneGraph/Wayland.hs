{-# LANGUAGE PatternSynonyms #-}
module Simula.NewCompositor.SceneGraph.Wayland where

import Control.Lens
import Control.Monad
import Control.Concurrent.MVar
import Data.Int
import Data.Typeable
import Data.Word
import Linear
import Linear.OpenGL

import Graphics.Rendering.OpenGL hiding (scale, Plane, translate)
import Graphics.GL (glEnable, glDisable, pattern GL_DEPTH_TEST) -- workaround, probably a user error
import Foreign hiding (void)
import Foreign.C

import Simula.WaylandServer
import Simula.MotorcarServer

import Simula.NewCompositor.Geometry
import Simula.NewCompositor.Utils
import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Types
import Simula.NewCompositor.Wayland.Output

data BaseWaylandSurfaceNode = BaseWaylandSurfaceNode {
  _waylandSurfaceNodeBase :: BaseDrawable,
  _waylandSurfaceNodeSurface :: MVar (Some WaylandSurface),
  _waylandSurfaceNodeSurfaceTransform :: MVar (M44 Float),
  _waylandSurfaceNodeDecorations :: WireframeNode,
  _waylandSurfaceNodeTextureCoords :: BufferObject,
  _waylandSurfaceNodeVertexCoords :: BufferObject,
  _waylandSurfaceNodeShader :: Program,
  _waylandSurfaceNodeAPosition, _waylandSurfaceNodeATexCoord :: AttribLocation,
  _waylandSurfaceNodeUMVPMatrix :: UniformLocation,
  _waylandSurfaceNodeMapped :: MVar Bool
  } deriving (Eq, Typeable)

data MotorcarSurfaceNode = MotorcarSurfaceNode {
  _motorcarSurfaceNodeBase :: BaseWaylandSurfaceNode,

  _motorcarSurfaceNodeDepthCompositedSurfaceShader :: Program,
  _motorcarSurfaceNodeDepthCompositedSurfaceBlitterShader :: Program,
  _motorcarSurfaceNodeClippingShader :: Program,
    
  _motorcarSurfaceNodeColorTextureCoords :: BufferObject,
  _motorcarSurfaceNodeDepthTextureCoords :: BufferObject,
  _motorcarSurfaceNodeSurfaceTextureCoords :: BufferObject,
  _motorcarSurfaceNodeCuboidClippingVertices :: BufferObject,
  _motorcarSurfaceNodeCuboidClippingIndices :: BufferObject,
  
  _motorcarSurfaceNodeAPositionDepthComposite:: AttribLocation,
  _motorcarSurfaceNodeAColorTexCoordDepthComposite:: AttribLocation,
  _motorcarSurfaceNodeADepthTexCoordDepthComposite:: AttribLocation,

  _motorcarSurfaceNodeAPositionBlit :: AttribLocation,
  _motorcarSurfaceNodeATexCoordBlit :: AttribLocation,
  _motorcarSurfaceNodeUColorSamplerBlit :: UniformLocation,
  _motorcarSurfaceNodeUDepthSamplerBlit :: UniformLocation,

  _motorcarSurfaceNodeAPositionClipping :: AttribLocation,
  _motorcarSurfaceNodeUMVPMatrixClipping :: UniformLocation,
  _motorcarSurfaceNodeUColorClipping :: UniformLocation,

  _motorcarSurfaceNodeDimensions :: MVar (V3 Float),

  _motorcarSurfaceNodeResource :: MVar (Maybe WlResource),
  _motorcarSurfaceNodeDimensionsArray :: WlArray,
  _motorcarSurfaceNodeTransformArray :: WlArray,
  _motorcarSurfaceNodePtr :: StablePtr MotorcarSurfaceNode
  } deriving (Eq, Typeable)



makeClassy ''BaseWaylandSurfaceNode
makeClassy ''MotorcarSurfaceNode

class Drawable a => WaylandSurfaceNode a where
  wsnSurface :: a -> IO (Some WaylandSurface)
  default wsnSurface :: HasBaseWaylandSurfaceNode a => a -> IO (Some WaylandSurface)
  wsnSurface = views waylandSurfaceNodeSurface readMVar
  
  setWsnSurface :: a -> (Some WaylandSurface) -> IO ()
  default setWsnSurface :: HasBaseWaylandSurfaceNode a => a -> (Some WaylandSurface) -> IO ()
  setWsnSurface = views waylandSurfaceNodeSurface writeMVar

  
  wsnMapped :: a -> IO Bool
  default wsnMapped :: HasBaseWaylandSurfaceNode a => a -> IO Bool
  wsnMapped = views waylandSurfaceNodeMapped readMVar
  
  setWsnMapped :: a -> Bool -> IO ()
  default setWsnMapped :: HasBaseWaylandSurfaceNode a => a -> Bool -> IO ()
  setWsnMapped = views waylandSurfaceNodeMapped writeMVar
  
  computeLocalSurfaceIntersection :: a -> Ray -> IO (Maybe (V2 Float, Float))
  default computeLocalSurfaceIntersection :: HasBaseWaylandSurfaceNode a => a -> Ray -> IO (Maybe (V2 Float, Float))
  computeLocalSurfaceIntersection this ray
    | dot (ray ^. rayPos) (surfacePlane ^. planeNorm) == 0
    = return Nothing
  
    | otherwise = do
        tfRay <- (transformRay ray . inv44) <$> readMVar (this ^. waylandSurfaceNodeSurfaceTransform)
        let t = intersectPlane surfacePlane tfRay
        let intersection = solveRay tfRay t
        Some ws <- readMVar (this ^. waylandSurfaceNodeSurface)
        size <- (fmap.fmap) fromIntegral $ wsSize ws
        let coords = liftI2 (*) intersection (V3 (size ^. _x) (size ^. _y) 0)
      
        return $ if t >= 0 then Just (coords ^. _xy, t) else Nothing

    where
      surfacePlane = Plane (V3 0 0 0) (V3 0 0 1)

  computeSurfaceTransform :: a -> Float -> IO ()
  default computeSurfaceTransform :: HasBaseWaylandSurfaceNode a => a -> Float -> IO ()
  computeSurfaceTransform this ppcm = when (ppcm > 0) $ do
    let ppm = 100*ppcm
    let rotQ = axisAngle (V3 0 0 1) (radians 180)
    let rotM = m33_to_m44 $ fromQuaternion rotQ
    
    Some surface <- readMVar (this ^. waylandSurfaceNodeSurface)
    size <- (fmap . fmap) fromIntegral $ wsSize surface
    let scaleM = scale (V3 (negate (size ^. _x) / ppm)  ((size ^. _y) / ppm) 1)
    let offsetM = translate (V3 (negate 0.5) (negate 0.5) 0)
          
    writeMVar (this ^. waylandSurfaceNodeSurfaceTransform) $ rotM !*! scaleM !*! offsetM
    
    setNodeTransform (this ^. waylandSurfaceNodeDecorations) $ rotM !*! scaleM !*! scale (V3 1.04 1.04 0)

instance HasBaseSceneGraphNode BaseWaylandSurfaceNode where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable BaseWaylandSurfaceNode where
  baseDrawable = waylandSurfaceNodeBase

instance HasBaseSceneGraphNode MotorcarSurfaceNode where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable MotorcarSurfaceNode where
  baseDrawable = baseWaylandSurfaceNode.baseDrawable

instance HasBaseWaylandSurfaceNode MotorcarSurfaceNode where
  baseWaylandSurfaceNode = motorcarSurfaceNodeBase

instance SceneGraphNode BaseWaylandSurfaceNode where
  nodeOnFrameBegin this _ = do
    checkForErrors
    computeSurfaceTransform this 8
    Some surface <- readMVar $ _waylandSurfaceNodeSurface this
    wsPrepare surface
    
  nodeOnFrameDraw = drawableOnFrameDraw

  nodeIntersectWithSurfaces this ray = do
    closestSubtreeIntersection <- defaultNodeIntersectWithSurfaces this ray
    Some surface <- readMVar $ this ^. waylandSurfaceNodeSurface
    ty <- wsType surface
    case ty of
      Cursor -> return closestSubtreeIntersection
      _ -> do
        tf <- nodeTransform this
        let localRay = transformRay ray (inv44 tf)

        maybeIsec <- computeLocalSurfaceIntersection this localRay

        size <- (fmap . fmap) fromIntegral $ wsSize surface

        case (maybeIsec, closestSubtreeIntersection) of
          (Just (isec, t), Just closest)
            | isec ^. _x >= 0 && isec ^. _y >= 0
              && and (liftI2 (<=) isec size)
              && t < (closest ^. rsiT)
            -> return . Just $ RaySurfaceIntersection (Some this) isec ray t
          _ -> return closestSubtreeIntersection
                             
      

instance VirtualNode BaseWaylandSurfaceNode

instance Drawable BaseWaylandSurfaceNode where
  drawableDraw this scene display = do
    putStrLn "this shouldn't be drawing"
    Some surface <- readMVar $ _waylandSurfaceNodeSurface this
    Just texture <- wsTexture surface

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

    surfaceTf <- readMVar $ _waylandSurfaceNodeSurfaceTransform this
    viewpoints <- readMVar $ _displayViewpoints display
    forM_ viewpoints $ \vp -> do
      --TODO compare w/ order in draw for WireFrameNode 
      port <- readMVar (vp ^. viewPointViewPort)
      setViewPort port
      
      projMatrix <- readMVar (vp ^. viewPointProjectionMatrix)
      viewMatrix <- readMVar (vp ^. viewPointViewMatrix)
      worldTf <- nodeWorldTransform this
    
      let mat = (projMatrix !*! viewMatrix !*! worldTf !*! surfaceTf) ^. m44GLmatrix
      uniform uMVPMatrix $= mat
      drawArrays TriangleFan 0 4
      checkForErrors

    textureBinding Texture2D $= Nothing
    vertexAttribArray aPosition $= Disabled
    vertexAttribArray aTexCoord $= Disabled
    currentProgram $= Nothing

instance WaylandSurfaceNode BaseWaylandSurfaceNode

instance SceneGraphNode MotorcarSurfaceNode where
  nodeOnWorldTransformChange this scene = sendTransformToClient this
  nodeOnFrameBegin this _ = do
    putStrLn "on frame begin"
    computeSurfaceTransform this 8
    Some surface <- readMVar $ this ^. waylandSurfaceNodeSurface
    wsPrepare surface
    
  nodeOnFrameDraw = drawableOnFrameDraw

instance VirtualNode MotorcarSurfaceNode

instance Drawable MotorcarSurfaceNode where
  drawableDraw this scene display = do
    putStrLn "drawing"
    checkForErrors
    stencilTest $= Enabled
    checkForErrors
    
    bindFramebuffer DrawFramebuffer $= display ^. displayScratchFrameBuffer
    checkForErrors
    
    clearColor $= Color4 0 0 0 0
    checkForErrors
    
    clearDepthf $= 1 --opengl es doesn't support clearDepth
    checkForErrors
    
    clearStencil $= 0
    checkForErrors
    
    stencilMask $= 0xff
    checkForErrors
    
    clear [ColorBuffer, DepthBuffer, StencilBuffer]
    checkForErrors
    
    drawWindowBoundsStencil this display
    checkForErrors
      
    Some surface <- readMVar (this ^. waylandSurfaceNodeSurface)
    dce <- wsDepthCompositingEnabled surface
    
    let surfaceCoords = this ^. motorcarSurfaceNodeSurfaceTextureCoords

    case dce of
      True -> do
        currentProgram $= Just (this ^. motorcarSurfaceNodeDepthCompositedSurfaceShader)
        let aPosition = this ^. motorcarSurfaceNodeAPositionDepthComposite
        let aColorTexCoord = this ^. motorcarSurfaceNodeAColorTexCoordDepthComposite
        let aDepthTexCoord = this ^. motorcarSurfaceNodeADepthTexCoordDepthComposite

        vertexAttribArray aPosition $= Enabled
        bindBuffer ArrayBuffer $= Just surfaceCoords
        vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  
        vertexAttribArray aColorTexCoord $= Enabled
        bindBuffer ArrayBuffer $= Nothing
        vertexAttribArray aDepthTexCoord $= Enabled
        bindBuffer ArrayBuffer $= Nothing
      _ -> do
        currentProgram $= Just (this ^. waylandSurfaceNodeShader)
        let aPosition = this ^. waylandSurfaceNodeAPosition
        let aTexCoord = this ^. waylandSurfaceNodeATexCoord
        let uMVPMatrix = this ^. waylandSurfaceNodeUMVPMatrix

        vertexAttribArray aPosition $= Enabled
        bindBuffer ArrayBuffer $= Just surfaceCoords
        vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)

        vertexAttribArray aTexCoord $= Enabled
        bindBuffer ArrayBuffer $= Nothing
        uniform uMVPMatrix $= (identity ^. m44GLmatrix :: GLmatrix Float)
        glDisable GL_DEPTH_TEST
        depthMask $= Disabled

    checkForErrors
    --TODO proper Nothing handling; this theoretically shouldn't happen
    Just tex <- wsTexture surface
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )
    checkForErrors
    
    vps <- readMVar (display ^. displayViewpoints)

    forM_ vps $ \vp -> do
      readMVar (vp ^. viewPointViewPort) >>= setViewPort
      case dce of
        True -> do
          ccvp <- readMVar (vp ^. viewPointClientColorViewPort)
          ccvpCoords <- vpCoords ccvp
          let aColorTexCoord = this ^. motorcarSurfaceNodeAColorTexCoordDepthComposite
          
          withArrayLen ccvpCoords $ \len coordPtr ->
            vertexAttribPointer aColorTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 coordPtr)

          cdvp <- readMVar (vp ^. viewPointClientDepthViewPort)
          cdvpCoords <- vpCoords cdvp
          let aDepthTexCoord = this ^. motorcarSurfaceNodeADepthTexCoordDepthComposite
          
          withArrayLen cdvpCoords $ \len coordPtr ->
            vertexAttribPointer aDepthTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 coordPtr)
        False -> do
          vport <- readMVar (vp ^. viewPointViewPort)
          vportCoords <- vpCoords vport
          let aTexCoord = this ^. waylandSurfaceNodeATexCoord
          
          withArrayLen vportCoords $ \len coordPtr ->
            vertexAttribPointer aTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 coordPtr)

      checkForErrors
      drawArrays TriangleFan 0 4
      checkForErrors

    when (not dce) $ do
      glEnable GL_DEPTH_TEST
      checkForErrors
      depthMask $= Enabled

    checkForErrors

    clipWindowBounds this display
    checkForErrors
      
    drawFrameBufferContents this display
    checkForErrors

    bindFramebuffer Framebuffer $= defaultFramebufferObject
    vertexAttribArray (this ^. motorcarSurfaceNodeAPositionDepthComposite) $= Disabled
    vertexAttribArray (this ^. motorcarSurfaceNodeAColorTexCoordDepthComposite) $= Disabled
    activeTexture $= TextureUnit 1
    textureBinding Texture2D $= Nothing
    checkForErrors
    
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Nothing
    checkForErrors
    
    currentProgram $= Nothing
    stencilTest $= Disabled
    checkForErrors
  
      where
        vpCoords :: ViewPort -> IO [Float]
        vpCoords vp = do
          vpOffset <- readMVar (vp ^. viewPortOffsetFactor)
          vpSize <- readMVar (vp ^. viewPortSizeFactor)
          return $ [ vpOffset ^. _x, 1 - vpOffset ^. _y
                   , vpOffset ^. _x + vpSize ^. _x, 1 - vpOffset ^. _y
                   , vpOffset ^. _x + vpSize ^. _x, 1 - vpOffset ^. _y - vpSize ^. _y
                   , vpOffset ^. _x, 1 - vpOffset ^. _y - vpSize ^. _y ]

        drawWindowBoundsStencil this display = do
          colorMask $= Color4 Disabled Disabled Disabled Disabled
          depthMask $= Disabled
          stencilFunc $= (Never, 1, 0xff)
          stencilOp $= (OpReplace, OpKeep, OpKeep)
          checkForErrors

          currentProgram $= Just (this ^. motorcarSurfaceNodeClippingShader)
          let aPosition = this ^. motorcarSurfaceNodeAPositionClipping
          let uMVPMatrix = this ^. motorcarSurfaceNodeUMVPMatrixClipping
          let uColor = this ^. motorcarSurfaceNodeUColorClipping

          vertexAttribArray aPosition $= Enabled
          bindBuffer ArrayBuffer $= Just (this ^. motorcarSurfaceNodeCuboidClippingVertices)
          vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
          checkForErrors

          uniform uColor $= (Color3 1 1 0 :: Color3 Float)

          bindBuffer ElementArrayBuffer $= Just (this ^. motorcarSurfaceNodeCuboidClippingIndices)

          wt <- nodeWorldTransform this
          dims <- readMVar (this ^. motorcarSurfaceNodeDimensions)
          let modelMat = wt !*! scale dims

          vps <- readMVar (display ^. displayViewpoints)
          forM_ vps $ \vp -> do
            port <- readMVar (vp ^. viewPointViewPort)
            setViewPort port

            projMatrix <- readMVar (vp ^. viewPointProjectionMatrix)
            viewMatrix <- readMVar (vp ^. viewPointViewMatrix)
            let mvp = (projMatrix !*! viewMatrix !*! modelMat) ^. m44GLmatrix
            uniform uMVPMatrix $= mvp
            let numElements = 36 --TODO eliminate this
            drawElements Triangles numElements UnsignedInt nullPtr
            checkForErrors
  
          vertexAttribArray aPosition $= Disabled
          currentProgram $= Nothing
          colorMask $= Color4 Enabled Enabled Enabled Enabled
          depthMask $= Enabled
          checkForErrors
          stencilMask $= 0
          stencilFunc $= (Equal, 1, 0xff)
          checkForErrors

        clipWindowBounds :: MotorcarSurfaceNode -> Display -> IO ()
        clipWindowBounds this display = do
          checkForErrors
          colorMask $= Color4 Disabled Disabled Disabled Disabled
          checkForErrors
          depthMask $= Disabled
          checkForErrors
          stencilMask $= 0xff
          checkForErrors
          stencilFunc $= (Always, 0, 0xff)
          checkForErrors
          stencilOp $= (OpKeep, OpKeep, OpReplace)
          checkForErrors

          currentProgram $= Just (this ^. motorcarSurfaceNodeClippingShader)
          let aPosition = this ^. motorcarSurfaceNodeAPositionClipping
          let uMVPMatrix = this ^. motorcarSurfaceNodeUMVPMatrixClipping
          let uColor = this ^. motorcarSurfaceNodeUColorClipping

          vertexAttribArray aPosition $= Enabled
          bindBuffer ArrayBuffer $= Just (this ^. motorcarSurfaceNodeCuboidClippingVertices)
          vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
          checkForErrors
          
          uniform uColor $= (Color3 1 0 0 :: Color3 Float)

          bindBuffer ElementArrayBuffer $= Just (this ^. motorcarSurfaceNodeCuboidClippingIndices)

          wt <- nodeWorldTransform this
          dims <- readMVar (this ^. motorcarSurfaceNodeDimensions)
          let modelMat = wt !*! scale dims

          vps <- readMVar (display ^. displayViewpoints)
          
          Some surface <- readMVar (this ^. waylandSurfaceNodeSurface)
          cmode <- wsClippingMode surface
          dce <- wsDepthCompositingEnabled surface

          when (cmode == Cuboid && dce) $ do
            cullFace $= Just Front
            forM_ vps $ \vp -> do
              port <- readMVar (vp ^. viewPointViewPort)
              setViewPort port
              
              projMatrix <- readMVar (vp ^. viewPointProjectionMatrix)
              viewMatrix <- readMVar (vp ^. viewPointViewMatrix)

              let mvp = (projMatrix !*! viewMatrix !*! modelMat) ^. m44GLmatrix
              uniform uMVPMatrix $= mvp
              let numElements = 36
              drawElements Triangles numElements UnsignedInt nullPtr
              checkForErrors
              
            cullFace $= Just Back

          if dce
            then depthFunc $= Just Greater
            else depthMask $= Enabled >> stencilMask $= 0
            
          forM_ vps $ \vp -> do
            port <- readMVar (vp ^. viewPointViewPort)
            setViewPort port
            
            projMatrix <- readMVar (vp ^. viewPointProjectionMatrix)
            viewMatrix <- readMVar (vp ^. viewPointViewMatrix)

            let mvp = (projMatrix !*! viewMatrix !*! modelMat) ^. m44GLmatrix
            uniform uMVPMatrix $= mvp
            let numElements = 36
            drawElements Triangles numElements UnsignedInt nullPtr
            checkForErrors


          depthFunc $= Just Less
          vertexAttribArray aPosition $= Disabled
          currentProgram $= Nothing
          colorMask $= Color4 Enabled Enabled Enabled Enabled
          depthMask $= Enabled
          stencilMask $= 0
          stencilFunc $= (Equal, 1, 0xff)
          checkForErrors
          
        drawFrameBufferContents this display = do
          depthFunc $= Just Lequal
          bindFramebuffer DrawFramebuffer $= defaultFramebufferObject
          bindFramebuffer ReadFramebuffer $= display ^. displayScratchFrameBuffer
          stencilMask $= 0xff

          let res = display ^. displaySize
          let s0 = Position 0 0
          let s1 = Position (fromIntegral $ res ^. _x - 1) (fromIntegral $ res ^. _y - 1)
          blitFramebuffer s0 s1 s0 s1 [StencilBuffer'] Nearest

          stencilMask $= 0
          stencilFunc $= (Equal, 1, 0xff)

          currentProgram $= Just (this ^. motorcarSurfaceNodeDepthCompositedSurfaceBlitterShader)
          
          activeTexture $= TextureUnit 0
          textureBinding Texture2D $= Just (display ^. displayScratchColorBufferTexture)
          textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )

          activeTexture $= TextureUnit 1
          textureBinding Texture2D $= Just (display ^. displayScratchDepthBufferTexture)
          textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )

          let aPosition = this ^. motorcarSurfaceNodeAPositionBlit
          let aTexCoord = this ^. motorcarSurfaceNodeATexCoordBlit
          let surfaceCoords = this ^. motorcarSurfaceNodeSurfaceTextureCoords

          vertexAttribArray aPosition $= Enabled
          bindBuffer ArrayBuffer $= Just surfaceCoords
          vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
          vertexAttribArray aTexCoord $= Enabled
          bindBuffer ArrayBuffer $= Nothing

          vps <- readMVar (display ^. displayViewpoints)
          forM_ vps $ \vp -> do
            vport <- readMVar (vp ^. viewPointViewPort)
            setViewPort vport
            
            vpOffset <- readMVar (vport ^. viewPortOffsetFactor)
            vpSize <- readMVar (vport ^. viewPortSizeFactor)
            let textureBlitCoords = [ vpOffset ^. _x, vpOffset ^. _y
                                    , vpOffset ^. _x + vpSize ^. _x, vpOffset ^. _y
                                    , vpOffset ^. _x + vpSize ^. _x, vpOffset ^. _y + vpSize  ^. _y
                                    , vpOffset ^. _x, vpOffset ^. _y + vpSize ^. _y ] :: [Float]

            withArrayLen textureBlitCoords $ \len coordPtr ->
              vertexAttribPointer aTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 coordPtr)

            drawArrays TriangleFan 0 4
            checkForErrors
     
      

instance WaylandSurfaceNode MotorcarSurfaceNode where
  computeLocalSurfaceIntersection this ray = do
    dims <- readMVar (this ^. motorcarSurfaceNodeDimensions)
    let box = AxisAlignedBox dims
    let t = intersectBox box ray 0 100
    return $ if t >= 0 then Just (V2 0 0, t) else Nothing
  
  computeSurfaceTransform this ppcm = writeMVar (this ^. baseWaylandSurfaceNode.waylandSurfaceNodeSurfaceTransform) identity

newWaylandSurfaceNode :: (WaylandSurface ws, SceneGraphNode a) => (Maybe (Some SceneGraphNode)) -> ws -> a -> M44 Float -> IO BaseWaylandSurfaceNode
newWaylandSurfaceNode maybeThis ws parent tf = do
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

  decoNode <- newWireframeNode decoVert decoColor Nothing identity

  rec node <- BaseWaylandSurfaceNode base
              <$> newMVar (Some ws)
              <*> newMVar identity
              <*> pure decoNode
              <*> pure texCoords
              <*> pure verCoords
              <*> pure program
              <*> pure aPos
              <*> pure aTex
              <*> pure uMVP
              <*> newMVar False
      base <- case maybeThis of
                Just (Some this) -> newBaseDrawable this (Just (Some parent)) tf
                _ -> newBaseDrawable node (Just (Some parent)) tf
  checkForErrors
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
      let V3 sx sy sz = corner & coord -~ (0.25 * direction ^. coord)
      in [cx, cy, cz, sx, sy, sz]



newMotorcarSurfaceNode :: (WaylandSurface ws, SceneGraphNode a) => ws -> a -> M44 Float -> V3 Float -> IO MotorcarSurfaceNode
newMotorcarSurfaceNode ws prt tf dims = do
  dcss <- getProgram ShaderDepthCompositedSurface
  dcsbs <-  getProgram ShaderDepthCompositedSurfaceBlitter
  clipping <- getProgram ShaderMotorcarLine

  surfaceTexCoords <- genObjectName
  bindBuffer ArrayBuffer $= Just surfaceTexCoords
  --TODO make this into an utility function
  withArrayLen surfaceVerts $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)
  checkForErrors

  currentProgram $= Just dcsbs
  colorSamplerBlit <- get $ uniformLocation dcsbs "uColorSampler"
  depthSamplerBlit <- get $ uniformLocation dcsbs "uDepthSampler"
  uniform colorSamplerBlit $= TextureUnit 0
  uniform depthSamplerBlit $= TextureUnit 1
  currentProgram $= Nothing
  checkForErrors

  ccv <- genObjectName
  bindBuffer ArrayBuffer $= Just ccv
  withArrayLen cuboidClippingVerts $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Word32)), coordPtr, StaticDraw)
  checkForErrors

  cci <- genObjectName
  bindBuffer ElementArrayBuffer $= Just cci
  withArrayLen cuboidClippingInds $ \len coordPtr ->
    bufferData ElementArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)
  checkForErrors

  rec node <- MotorcarSurfaceNode wsn
              <$> pure dcss
              <*> pure dcsbs
  
              <*> pure clipping
              <*> genObjectName
              <*> genObjectName
              <*> pure surfaceTexCoords
              <*> pure ccv
              <*> pure cci

              <*> get (attribLocation dcss "aPosition")
              <*> get (attribLocation dcss "aColorTexCoord")
              <*> get (attribLocation dcss "aDepthTexCoord")

              <*> get (attribLocation dcsbs "aPosition")
              <*> get (attribLocation dcsbs "aTexCoord")
              <*> pure colorSamplerBlit
              <*> pure depthSamplerBlit

              <*> get (attribLocation clipping "aPosition")
              <*> get (uniformLocation clipping "uMVPMatrix")
              <*> get (uniformLocation clipping "uColor")

              <*> newMVar dims
              <*> newMVar Nothing
              <*> newWlArray
              <*> newWlArray
              <*> newStablePtr node
      wsn <- newWaylandSurfaceNode (Just (Some node)) ws prt tf

  setNodeTransform (node ^. waylandSurfaceNodeDecorations) $ scale dims
  setNodeParent (node ^. waylandSurfaceNodeDecorations) (Just (Some node))
  return node
    
    
  where
    surfaceVerts = [-1, -1, 0, 1, -1, 0, 1, 1, 0, -1, 1, 0] :: [Float]
    cuboidClippingVerts = [ 0.5, 0.5, 0.5, 0.5, 0.5,-0.5
                          , 0.5,-0.5, 0.5, 0.5,-0.5,-0.5
                          ,-0.5, 0.5, 0.5,-0.5, 0.5,-0.5
                          ,-0.5,-0.5, 0.5,-0.5,-0.5,-0.5
                          ] :: [Float]
    cuboidClippingInds = [0,2,1,1,2,3,4,5,6,5,7,6,0,1,4,1,5,4,2,6,3,3,6,7,0,4,2,2,4,6,1,3,5,3,7,5] :: [Word32]
    

sendTransformToClient :: MotorcarSurfaceNode -> IO ()
sendTransformToClient this = do
  resource <- readMVar (this ^. motorcarSurfaceNodeResource)
  case resource of
    Just resource -> do
      worldTf <- nodeWorldTransform this
      --TODO verify
      let columnMajorMat = transpose worldTf
      let array = this ^. motorcarSurfaceNodeTransformArray
      wlArrayOverwrite array columnMajorMat
      motorcar_surface_send_transform_matrix resource array
    _ -> return ()
      

setMsnDimensions :: MotorcarSurfaceNode -> V3 Float -> IO ()
setMsnDimensions this dims = do
  Some surface <- readMVar (this ^. waylandSurfaceNodeSurface)
  cmode <- wsClippingMode surface
  let dims' = if cmode == Portal then dims & _z .~ 0 else dims

  writeMVar (this ^. motorcarSurfaceNodeDimensions) dims'
  let decoNode = this ^. waylandSurfaceNodeDecorations
  setNodeTransform decoNode $ scale dims'

configureResource :: MotorcarSurfaceNode -> WlClient -> CUInt -> IO ()
configureResource this client ident = do
  surfaceIf <- motorcarSurfaceInterface
  surfaceVer <- motorcarSurfaceVersion
  res <- wl_resource_create client surfaceIf surfaceVer (fromIntegral ident)
  writeMVar (this ^. motorcarSurfaceNodeResource) (Just res)

  sFuncPtr <- createSetSize3DFuncPtr setSize3D
  sFuncPtrPtr <- castPtr <$> new sFuncPtr

  let nodePtr = castStablePtrToPtr (this ^. motorcarSurfaceNodePtr)
  
  wl_resource_set_implementation res sFuncPtrPtr nodePtr nullFunPtr

  sendTransformToClient this
  readMVar (this ^. motorcarSurfaceNodeDimensions) >>= msnRequestSize3D this
  putStrLn "Configured motorcar surface"

  where
    setSize3D client resource dimsArr = do
      nodePtr <- castPtrToStablePtr <$> wlResourceData resource
      node <- deRefStablePtr nodePtr
      size <- wlArraySize dimsArr

      when (fromIntegral size /= sizeOf (undefined :: V3 Float)) . ioError $ userError "Invalid dimensions array size"
      
      dims <- wlArrayData dimsArr >>= peek
      
      Some surface <- readMVar (node ^. waylandSurfaceNodeSurface)
      cmode <- wsClippingMode surface
      let dims' = if cmode == Portal then dims & _z .~ 0 else dims
      setMsnDimensions node dims'

msnRequestSize3D :: MotorcarSurfaceNode -> V3 Float -> IO ()
msnRequestSize3D this dims = do
  Some surface <- readMVar (this ^. waylandSurfaceNodeSurface)
  cmode <- wsClippingMode surface
  let dims' = if cmode == Portal then dims & _z .~ 0 else dims

  resource <- readMVar (this ^. motorcarSurfaceNodeResource)
  case resource of
    Just resource -> do
      let array = this ^. motorcarSurfaceNodeDimensionsArray
      wlArrayOverwrite array dims'
      motorcar_surface_send_request_size_3d resource array
    Nothing -> setMsnDimensions this dims'

destroyMotorcarSurfaceNode :: MotorcarSurfaceNode -> IO ()
destroyMotorcarSurfaceNode this = do
  res <- readMVar (this ^. motorcarSurfaceNodeResource)
  case res of
    Just res -> wl_resource_destroy res
    _ -> return ()

  wl_array_release (this ^. motorcarSurfaceNodeDimensionsArray)
  wl_array_release (this ^. motorcarSurfaceNodeTransformArray)
  freeStablePtr (this ^. motorcarSurfaceNodePtr)
