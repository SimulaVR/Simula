module Simula.NewCompositor.Shell where

import Control.Lens
import Data.IORef
import Data.Typeable
import Foreign
import Foreign.C

import Simula.MotorcarServer
import Simula.WaylandServer

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.SceneGraph.Wayland
import Simula.NewCompositor.Types
import Simula.NewCompositor.Wayland.Output
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.Utils



data Shell = Shell {
  _shellScene :: IORef Scene,
  _shellDisplay :: IORef WlDisplay,
  _shellGlobal :: WlGlobal,
  _shellPtr :: StablePtr Shell,
  _shellBindFunc :: FunPtr GlobalBindFunc
  }

makeLenses ''Shell

newShell :: Scene -> IO Shell
newShell scene = do
  Some comp <- readIORef (scene ^. sceneCompositor)
  let dp = compositorWlDisplay comp
  rec shell <- Shell <$> newIORef scene <*> newIORef dp
        <*> pure global <*> pure shellPtr <*> pure bindFuncPtr
      shellPtr <- newStablePtr shell

      shellIf <- motorcarShellInterface
      shellVer <- motorcarShellVersion

      bindFuncPtr <- createGlobalBindFuncPtr bindFunc
  
      global <- wl_global_create dp shellIf shellVer (castStablePtrToPtr shellPtr) bindFuncPtr
  return shell

  where
    bindFunc client shell version ident = do
      shellIf <- motorcarShellInterface
      resource <- wl_resource_create client shellIf (fromIntegral version) (fromIntegral ident)
      sFuncPtr <- createGetMotorcarSurfaceFuncPtr getMotorcarSurface
      sFuncPtrPtr <- castPtr <$> new sFuncPtr
      rec dFuncPtr <- createResourceDestroyFuncPtr (destroyFunc dFuncPtr sFuncPtrPtr)
      wl_resource_set_implementation resource sFuncPtrPtr shell dFuncPtr

    destroyFunc dFuncPtr sFuncPtrPtr _ = do
      peek (castPtr sFuncPtrPtr) >>= freeHaskellFunPtr 
      freeHaskellFunPtr dFuncPtr

    getMotorcarSurface client resource ident surfaceResource clipmode dce = do
      shellPtr <- castPtrToStablePtr <$> wlResourceData resource
      shell <- deRefStablePtr shellPtr
      
      scene <- readIORef (shell ^. shellScene)
      Some comp <- readIORef (scene ^. sceneCompositor)
      Some surface <- compositorGetSurfaceFromResource comp surfaceResource

      let mode = case toEnum (fromIntegral clipmode) of
                   MotorcarCuboid -> Cuboid
                   MotorcarPortal -> Portal

      setWsClippingMode surface mode
      setWsIsMotorcarSurface surface True
      setWsDepthCompositingEnabled surface (toBool dce)
      Some wm <- readIORef (scene ^. sceneWindowManager)

      Some wsn <- wmCreateSurface wm surface
      --TODO this is silly, refactor
      case cast wsn of
        Just msn -> configureResource msn client ident
        Nothing -> ioError $ userError "wmCreateSurface didn't return MotorcarSurface"
      
      
destroyShell :: Shell -> IO ()
destroyShell shell = do
  freeHaskellFunPtr (shell ^. shellBindFunc)
  freeStablePtr (shell ^. shellPtr)

