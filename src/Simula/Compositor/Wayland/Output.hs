module Simula.Compositor.Wayland.Output where

import Control.Lens
import Control.Concurrent.MVar
import Data.Word
import Data.Hashable
import Data.Typeable
import Linear

import Debug.Trace

import Graphics.Rendering.OpenGL hiding (Proxy)

import {-# SOURCE #-} Simula.Compositor.Event
import Simula.Compositor.Types
import Simula.Compositor.Utils

data WaylandSurfaceType = TopLevel | Transient | Popup | Cursor | NA
  deriving (Show, Eq, Ord, Enum)

data WaylandSurfaceClippingMode = None | Cuboid | Portal
  deriving (Show, Eq, Ord, Enum)

data BaseWaylandSurface = BaseWaylandSurface {
  _baseWaylandSurfaceType :: MVar WaylandSurfaceType,
  _baseWaylandSurfaceClippingMode :: MVar WaylandSurfaceClippingMode,
  _baseWaylandSurfaceDepthCompositingEnabled :: MVar Bool,
  _baseWaylandSurfaceIsMotorcarSurface :: MVar Bool
  } deriving Eq

makeClassy ''BaseWaylandSurface


newBaseWaylandSurface :: WaylandSurfaceType -> IO BaseWaylandSurface
newBaseWaylandSurface ty = BaseWaylandSurface 
                           <$> newMVar ty <*> newMVar None
                           <*> newMVar False <*> newMVar False

class (Eq a, Hashable a, Typeable a) => WaylandSurface a where
  wsTexture :: a -> IO (Maybe TextureObject)
  wsSize :: a -> IO (V2 Int)
  setWsSize :: a -> V2 Int -> IO ()
  wsPosition :: a -> IO (V2 Float)
--  wsParentSurface :: a -> MVar (Some WaylandSurface)
  wsPrepare :: a -> IO ()
  wsSendEvent :: a -> InputEvent -> IO ()

  wsType :: a -> IO WaylandSurfaceType
  default wsType :: HasBaseWaylandSurface a => a -> IO WaylandSurfaceType
  wsType = views baseWaylandSurfaceType readMVar
  
  setWsType :: a -> WaylandSurfaceType -> IO ()
  default setWsType :: HasBaseWaylandSurface a => a -> WaylandSurfaceType -> IO ()
  setWsType = views baseWaylandSurfaceType writeMVar
  
  wsClippingMode :: a -> IO WaylandSurfaceClippingMode
  default wsClippingMode :: HasBaseWaylandSurface a => a -> IO WaylandSurfaceClippingMode
  wsClippingMode = views baseWaylandSurfaceClippingMode readMVar
  
  setWsClippingMode :: a -> WaylandSurfaceClippingMode -> IO ()
  default setWsClippingMode :: HasBaseWaylandSurface a => a -> WaylandSurfaceClippingMode -> IO ()
  setWsClippingMode = views baseWaylandSurfaceClippingMode writeMVar
  
  wsDepthCompositingEnabled :: a -> IO Bool
  default wsDepthCompositingEnabled :: HasBaseWaylandSurface a => a -> IO Bool
  wsDepthCompositingEnabled = views baseWaylandSurfaceDepthCompositingEnabled readMVar
  
  setWsDepthCompositingEnabled :: a -> Bool -> IO ()
  default setWsDepthCompositingEnabled :: HasBaseWaylandSurface a => a -> Bool -> IO ()
  setWsDepthCompositingEnabled = views baseWaylandSurfaceDepthCompositingEnabled writeMVar

  wsIsMotorcarSurface :: a -> IO Bool
  default wsIsMotorcarSurface :: HasBaseWaylandSurface a => a -> IO Bool
  wsIsMotorcarSurface = views baseWaylandSurfaceIsMotorcarSurface readMVar
  
  setWsIsMotorcarSurface :: a -> Bool -> IO ()
  default setWsIsMotorcarSurface :: HasBaseWaylandSurface a => a -> Bool -> IO ()
  setWsIsMotorcarSurface = views baseWaylandSurfaceIsMotorcarSurface writeMVar

instance Eq (Some WaylandSurface) where
  Some a == Some b = case cast a of
    Just a -> a == b
    _ -> False

instance Hashable (Some WaylandSurface) where
  hashWithSalt s (Some x) = hashWithSalt s x
