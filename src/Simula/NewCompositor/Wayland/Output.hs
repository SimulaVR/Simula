module Simula.NewCompositor.Wayland.Output where

import Control.Lens
import Control.Concurrent.MVar
import Data.Word
import Data.Unique
import Data.Typeable
import Linear

import Graphics.Rendering.OpenGL hiding (Proxy)

import {-# SOURCE #-} Simula.NewCompositor.Event
import Simula.NewCompositor.Types
import Simula.NewCompositor.Utils

data WaylandSurfaceType = TopLevel | Transient | Popup | Cursor | NA
  deriving (Show, Eq, Ord, Enum)

data WaylandSurfaceClippingMode = None | Cuboid | Portal
  deriving (Show, Eq, Ord, Enum)

data BaseWaylandSurface = BaseWaylandSurface {
  _baseWaylandSurfaceId :: Unique, -- hack for ord instance
  _baseWaylandSurfaceType :: MVar WaylandSurfaceType,
  _baseWaylandSurfaceClippingMode :: MVar WaylandSurfaceClippingMode,
  _baseWaylandSurfaceDepthCompositingEnabled :: MVar Bool,
  _baseWaylandSurfaceIsMotorcarSurface :: MVar Bool
  } deriving Eq

makeClassy ''BaseWaylandSurface

instance Ord BaseWaylandSurface where
  x <= y = (x ^. baseWaylandSurfaceId) <= (y ^. baseWaylandSurfaceId)

newBaseWaylandSurface :: WaylandSurfaceType -> IO BaseWaylandSurface
newBaseWaylandSurface ty = BaseWaylandSurface <$> newUnique
                           <*> newMVar ty <*> newMVar None
                           <*> newMVar False <*> newMVar False

class (Eq a, Typeable a) => WaylandSurface a where
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

  wsId :: a -> Unique
  default wsId :: HasBaseWaylandSurface a => a -> Unique
  wsId = view baseWaylandSurfaceId
  
instance Eq (Some WaylandSurface) where
  Some a == Some b = case cast a of
    Just a -> a == b
    _ -> False

instance Ord (Some WaylandSurface) where
  Some a <= Some b = wsId a <= wsId b
