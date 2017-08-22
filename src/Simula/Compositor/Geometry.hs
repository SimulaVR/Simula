module Simula.Compositor.Geometry where

import Control.Lens
import Linear

import {-# SOURCE #-} Simula.Compositor.SceneGraph.Wayland
import Simula.Compositor.Types

data Ray = Ray {
  _rayPos :: V3 Float,
  _rayDir :: V3 Float
  } deriving (Show, Eq, Ord)

makeLenses ''Ray

transformRay :: Ray -> M44 Float -> Ray
transformRay (Ray p d) t = Ray (p' ^. _xyz) (d' ^. _xyz)
  where
    p' = t !* point p
    d' = t !* vector d

solveRay :: Ray -> Float -> V3 Float
solveRay (Ray p d) t = p ^+^ (t *^ d)

data Plane = Plane {
  _planePos :: V3 Float,
  _planeNorm :: V3 Float
  } deriving (Show, Eq, Ord)

makeLenses ''Plane

intersectPlane :: Plane -> Ray -> Float
intersectPlane (Plane pp pn) (Ray rp rd) = ((pp - rp) `dot` pn) / (rd `dot` pn)

data AxisAlignedBox = AxisAlignedBox {
  _axisAlignedBoxDimensions :: V3 Float
  } deriving (Show, Eq, Ord)

makeLenses ''AxisAlignedBox

intersectBox :: AxisAlignedBox -> Ray -> Float -> Float -> Float
intersectBox (AxisAlignedBox bd) (Ray rp rd) t0 t1
  | tmin _x > tmax _y || tmin _y > tmax _x = -1
  | tminx' > tmax _z || tmin _z > tmaxx' = -1
  | tminx'' < t1 && tmaxx'' > t0 = tminx''
  | otherwise = -1
  where
    minVertex = bd ^* negate 0.5
    maxVertex = bd ^* 0.5

    tmin field | rd ^. field >= 0 = (minVertex ^. field - rp ^. field)/(rd ^. field)
               | otherwise        = (maxVertex ^. field - rp ^. field)/(rd ^. field)

    tmax field | rd ^. field >= 0 = (maxVertex ^. field - rp ^. field)/(rd ^. field)
               | otherwise        = (minVertex ^. field - rp ^. field)/(rd ^. field)

    tminx' = max (tmin _x) (tmin _y)
    tmaxx' = min (tmax _x) (tmax _y)

    tminx'' = max tminx' (tmin _z)
    tmaxx'' = min tmaxx' (tmax _z)

--TODO not actually a rectangle, refactor
data Rectangle = Rectangle {
  _rectangleSize :: V2 Int
  } deriving (Show, Eq, Ord)

makeLenses ''Rectangle

data RaySurfaceIntersection = RaySurfaceIntersection {
  _rsiSurfaceNode :: Some WaylandSurfaceNode,
  _rsiSurfaceCoordinates :: V2 Float,
  _rsiRay :: Ray,
  _rsiT :: Float
  }

makeLenses ''RaySurfaceIntersection

data Rect a = Rect {
  _rectUpperLeft :: V2 a,
  _rectSize :: V2 a
  }

makeLenses ''Rect

rectFromPosSize :: V2 a -> V2 a -> Rect a
rectFromPosSize = Rect

rectTop, rectBottom, rectLeft, rectRight :: Num a => Rect a -> a
rectTop = view (rectUpperLeft._y)
rectLeft = view (rectUpperLeft._x)
rectBottom rect = rectTop rect - (rect ^. rectSize._x)
rectRight rect = rectLeft rect + (rect ^. rectSize._y)
