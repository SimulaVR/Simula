module Simula.NewCompositor.SceneGraph where

import Data.IORef
import Data.Vector.Mutable

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.Types


data Scene = Scene {
  _sceneCurrentTimestamp :: IORef Int,
  _sceneLastTimestamp :: IORef Int,
  _sceneWindowManager :: IORef (Some WindowManager),
  _sceneCompositor :: IORef (Some Compositor),
  _sceneTrash :: IORef Scene,
  _sceneDisplays :: IOVector Display,
  _sceneActiveDisplay :: IORef Display
  }
