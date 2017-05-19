{-# LANGUAGE LambdaCase #-}
module Simula.NewCompositor.WindowManager where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad

import qualified Data.HashMap.Strict as HM
import Data.Typeable
import Foreign
import Foreign.C
import Linear

import Simula.WaylandServer
import Simula.MotorcarServer

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.Event
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.SceneGraph.Wayland
import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Wayland.Output
import Simula.NewCompositor.Types
import Simula.NewCompositor.Utils

data WindowManager = WindowManager {
  _windowManagerScene :: Scene,
  _windowManagerShell :: Shell,
  _windowManagerDefaultSeat :: Some Seat,
  _windowManagerNumSurfacesMapped :: MVar Int,
  _windowManagerSurfaceMap :: MVar (HM.HashMap (Some WaylandSurface) (Some WaylandSurfaceNode))
  } deriving (Eq, Typeable)

data Shell = Shell {
  _shellScene :: Scene,
  _shellDisplay :: WlDisplay,
  _shellGlobal :: WlGlobal,
  _shellPtr :: StablePtr Shell,
  _shellBindFunc :: FunPtr GlobalBindFunc
  } deriving (Eq, Typeable)

makeLenses ''WindowManager
makeLenses ''Shell

newWindowManager :: Seat st => Scene -> st -> IO WindowManager
newWindowManager scene seat = WindowManager scene <$> newShell scene
                              <*> pure (Some seat) <*> newMVar 0 <*> newMVar HM.empty

destroyWindowManager :: WindowManager -> IO ()
destroyWindowManager this = do
  --     delete m_defaultSeat;
  destroyShell (this ^. windowManagerShell)

wmCreateSurface :: WaylandSurface ws => WindowManager -> ws -> IO (Some WaylandSurfaceNode)
wmCreateSurface this surface = do
  putStrLn "creating surface"
  let ssurf = Some surface
  surfaceMap <- readMVar (this ^. windowManagerSurfaceMap) 
  case HM.lookup ssurf surfaceMap of
    Just snode@(Some node) ->  setWsnSurface node ssurf >>  return snode
    Nothing -> do
      isMSN <- wsIsMotorcarSurface surface
      let scene = this ^. windowManagerScene
      snode <- if isMSN
        then Some <$> newMotorcarSurfaceNode surface scene identity (V3 1 1 1)
        else Some <$> newWaylandSurfaceNode Nothing surface scene identity
      modifyMVar' (this ^. windowManagerSurfaceMap) (HM.insert ssurf snode)
      putStrLn "created surface"
      return snode

wmDestroySurface :: WaylandSurface ws => WindowManager -> ws -> IO ()
wmDestroySurface this surface = do
  surfaceMap <- readMVar (this ^. windowManagerSurfaceMap)
  let ssurf = Some surface
  case HM.lookup ssurf surfaceMap of
    Nothing -> return ()
    Just snode@(Some node) -> do
      nodeDestroy node
      cds <- readMVar (nodeChildren node)
      forM_ cds $ \case
        Some child -> case cast child of --TODO hacky
          Just (msn :: MotorcarSurfaceNode) -> destroySurface msn
          _ -> case cast child of
            Just (wsn :: BaseWaylandSurfaceNode) -> destroySurface wsn
            _ -> return ()

      modifyMVar' (this ^. windowManagerSurfaceMap) (HM.delete ssurf)
      case cast node of
        Just msn -> destroyMotorcarSurfaceNode msn
        _ -> return ()
  
      case this ^. windowManagerDefaultSeat of
        Some seat -> do
          ptr <- seatPointer seat
          writeMVar (ptr ^. pointerCursorNode) Nothing
          wmEnsureKeyboardFocusIsValid this surface
  where
    destroySurface :: WaylandSurfaceNode a => a -> IO ()
    destroySurface sn = do
      Some surface <- wsnSurface sn
      wmDestroySurface this surface
  

wmMapSurface :: WaylandSurface ws => WindowManager -> ws -> WaylandSurfaceType -> IO (Some WaylandSurfaceNode)
wmMapSurface this surface sty = do
  setWsType surface sty
  snode@(Some node) <- wmCreateSurface this surface

  let scene = this ^. windowManagerScene

  isMsn <- wsIsMotorcarSurface surface
  when isMsn $ case cast node of
    Just msn -> do
      mode <- wsClippingMode surface
      case mode of
        Cuboid -> msnRequestSize3D msn (V3 0.5 0.5 0.5)
        Portal -> msnRequestSize3D msn (V3 0.7 0.5 0)
      dce <- wsDepthCompositingEnabled surface
      Some comp <- readMVar (scene ^. sceneCompositor)
      dp <- compositorDisplay comp
      let size = dp ^. displaySize
      
      setWsSize surface $ if dce then size & _y *~ 2 else size
    Nothing -> ioError $ userError "Expected MotorcarSurfaceNode, but got something else"

  case sty of
    TopLevel -> do
      setNodeParent node (Just (Some scene))

      numSurfaces <- readMVar (this ^. windowManagerNumSurfacesMapped)
      let rotQ = axisAngle (V3 0 1 0) (radians $ (fromIntegral $ numSurfaces - 1) * thetaOffset)
      let rotM = m33_to_m44 $ fromQuaternion rotQ
      
      setNodeTransform node $ translate (V3 0 0 1) !*! rotM !*! translate (V3 0 0 zOffset)
      modifyMVar' (this ^. windowManagerNumSurfacesMapped) (+1)

      Some seat <- pure (this ^. windowManagerDefaultSeat)
      setSeatPointerFocus seat surface (V2 0 0)
    Popup -> handlePopupTransient node 
    Transient -> handlePopupTransient node
    _ -> return ()

  setWsnMapped node True
  return snode

  where
    
    popupZOffset = 0.05
    zOffset = -1.5
    thetaOffset = -30

    handlePopupTransient :: WaylandSurfaceNode a => a -> IO ()
    handlePopupTransient node = do
      Some seat <- pure (this ^. windowManagerDefaultSeat)
      focus <- seatPointer seat >>= views pointerFocus readMVar
        
      case focus of
        Just focus -> do
          focusNode <- HM.lookup focus <$> readMVar (this ^. windowManagerSurfaceMap)
          case focusNode of
            Just (Some focusNode) -> do
              localPos <- case sty of
                Popup -> seatPointer seat >>= views pointerLocalPosition readMVar
                _ -> wsPosition surface
              tf <- nodeTransform focusNode
              size <- (fmap.fmap) fromIntegral $ wsSize surface
              Some parentSurface <- wsnSurface focusNode
              parentSize <- (fmap.fmap) fromIntegral $ wsSize parentSurface
              let vec = (V4 0 0 popupZOffset 1) & _xy .~ liftI2 (/) (localPos + size ^/ 2) parentSize
              let pos = (tf !* vec) ^. _xyz
              setNodeTransform node (translate pos)
              setNodeParent node (Just (Some focusNode))
            Nothing -> ioError $ userError "parent of pointer focus missing"
        Nothing -> do
          setNodeParent node (Just (Some (this ^. windowManagerScene)))
          setNodeTransform node identity
      when (sty == Popup) $ setSeatPointerFocus seat surface (V2 0 0)
          
      
wmUnmapSurface :: WaylandSurface ws => WindowManager -> ws -> IO ()
wmUnmapSurface this surface = do
  surfaceMap <- readMVar (this ^. windowManagerSurfaceMap)
  case HM.lookup (Some surface) surfaceMap of
    Just (Some node) -> setWsnMapped node False >> wmEnsureKeyboardFocusIsValid this surface
    Nothing -> return ()

wmSendEvent :: WindowManager -> InputEvent -> IO ()
wmSendEvent this event = case event of
  MouseEvent (Some seat) _ -> do
    focus <- seatPointer seat >>= views pointerFocus readMVar
    case focus of
      Just (Some focus) -> wsSendEvent focus event
      _ -> return ()
  KeyboardEvent (Some seat) _ -> do
    focus <- seatKeyboard seat >>= views keyboardFocus readMVar
    case focus of
      Just (Some focus) -> wsSendEvent focus event
      _ -> return ()

wmEnsureKeyboardFocusIsValid :: WaylandSurface ws => WindowManager -> ws -> IO ()
wmEnsureKeyboardFocusIsValid this oldSurface = do
  surfaceMap <- readMVar (this ^. windowManagerSurfaceMap)
  --TODO ensure this can't fail
  nextSurface <- foldM getToplevel Nothing (HM.toList surfaceMap)

  Some seat <- pure (this ^. windowManagerDefaultSeat)
  seatEnsureKeyboardFocusIsValid seat oldSurface nextSurface

  where
    getToplevel :: Maybe (Some WaylandSurface) -> (Some WaylandSurface, Some WaylandSurfaceNode) -> IO (Maybe (Some WaylandSurface))
    getToplevel Nothing (Some ws, _) = do
      ty <- wsType ws
      return $ if ty == TopLevel then Just (Some ws) else Nothing
    getToplevel res _ = return res


newShell :: Scene -> IO Shell
newShell scene = do
  Some comp <- readMVar (scene ^. sceneCompositor)
  let dp = compositorWlDisplay comp
  rec let shell = Shell scene dp global shellPtr bindFuncPtr
      shellPtr <- newStablePtr shell

      shellIf <- motorcarShellInterface
      shellVer <- motorcarShellVersion

      bindFuncPtr <- createGlobalBindFuncPtr bindFunc
  
      global <- wl_global_create dp shellIf shellVer (castStablePtrToPtr shellPtr) bindFuncPtr
  putStrLn "Created shell"
  return shell

  where
    bindFunc client shell version ident = do
      shellIf <- motorcarShellInterface
      resource <- wl_resource_create client shellIf (fromIntegral version) (fromIntegral ident)
      sFuncPtr <- createGetMotorcarSurfaceFuncPtr getMotorcarSurface
      sFuncPtrPtr <- castPtr <$> new sFuncPtr
      wl_resource_set_implementation resource sFuncPtrPtr shell nullFunPtr
      putStrLn "Bound resource"

    getMotorcarSurface client resource ident surfaceResource clipmode dce = do
      putStrLn "Getting motorcar surface"
      shellPtr <- castPtrToStablePtr <$> wlResourceData resource
      shell <- deRefStablePtr shellPtr
      
      let scene = shell ^. shellScene
      Some comp <- readMVar (scene ^. sceneCompositor)
      Some surface <- compositorGetSurfaceFromResource comp surfaceResource

      let mode = case toEnum (fromIntegral clipmode) of
                   MotorcarCuboid -> Cuboid
                   MotorcarPortal -> Portal

      setWsClippingMode surface mode
      setWsIsMotorcarSurface surface True
      setWsDepthCompositingEnabled surface (toBool dce)
      let wm = scene ^. sceneWindowManager

      putStrLn "Creating motorcar surface"
      Some wsn <- wmCreateSurface wm surface
      putStrLn "Created motorcar surface"
      --TODO this is silly, refactor
      case cast wsn of
        Just msn -> configureResource msn client ident
        Nothing -> ioError $ userError "wmCreateSurface didn't return MotorcarSurface"
      
      
destroyShell :: Shell -> IO ()
destroyShell shell = do
  freeHaskellFunPtr (shell ^. shellBindFunc)
  freeStablePtr (shell ^. shellPtr)

