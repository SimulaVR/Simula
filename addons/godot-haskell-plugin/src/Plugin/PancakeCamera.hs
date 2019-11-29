{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.PancakeCamera where

import Control.Exception

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import           Godot.Nativescript
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import Plugin.Types
import Data.Maybe
import Data.Either

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Control.Lens                hiding (Context)

import Data.Typeable

import qualified Data.Map.Strict as M

instance Eq GodotPancakeCamera  where
  (==) = (==) `on` _gpcObject

instance NativeScript GodotPancakeCamera where
  className = "PancakeCamera"
  classInit node = do
    return $ GodotPancakeCamera (safeCast node)
  classMethods =
    [
      func NoRPC "_process" Plugin.PancakeCamera._process
    , func NoRPC "_ready" Plugin.PancakeCamera._ready
    ]

instance HasBaseClass GodotPancakeCamera where
  type BaseClass GodotPancakeCamera = GodotCamera
  super (GodotPancakeCamera obj ) = GodotCamera obj

_ready :: GodotPancakeCamera -> [GodotVariant] -> IO ()
_ready gpc args = do
  return ()

-- | Take the PoV of the ARVRCamera, if it exists.
_process :: GodotPancakeCamera -> [GodotVariant] -> IO ()
_process gpc args = do
  maybeARVRTransform <- getARVRCameraTransform gpc
  case maybeARVRTransform of
    (Just transform) -> G.set_global_transform gpc transform
    Nothing -> return ()
  return ()
  where getARVRCameraTransform :: GodotPancakeCamera -> IO (Maybe GodotTransform)
        getARVRCameraTransform gpc = do
          let nodePathStr = "/root/Root/VRViewport/ARVROrigin/ARVRCamera"
          nodePath <- (toLowLevel (pack nodePathStr))
          hasNode  <- G.has_node ((safeCast gpc) :: GodotNode) nodePath
          maybeTransform <- case hasNode of
                False -> do
                  putStrLn $ "Failed to get ARVRCamera transform for GodotPancakeCamera!"
                  return Nothing
                True ->  do arvrCameraNode  <- G.get_node ((safeCast gpc) :: GodotNode) nodePath
                            let arvrCamera = (coerce arvrCameraNode) :: GodotARVRCamera -- HACK: We use `coerce` instead of something more proper
                            transform <- G.get_global_transform (arvrCamera)
                            return (Just transform)
          return maybeTransform

