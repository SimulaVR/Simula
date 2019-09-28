module Plugin.Pointer
  ( defaultPointer
  )
where

import           Data.Colour
import           Data.Colour.SRGB

import           Plugin.Imports

import qualified Godot.Methods                 as G
import           Godot.Core.GodotSpatialMaterial
import           Plugin.Types

defaultPointer :: IO GodotMeshInstance
defaultPointer = do
  cube <- unsafeInstance GodotCubeMesh "CubeMesh"
  G.set_size cube =<< toLowLevel (V3 0.002 0.002 10)
  G.set_material cube =<< safeCast <$> laserMat

  mi <- unsafeInstance GodotMeshInstance "MeshInstance"
  G.set_mesh mi (safeCast cube)
  G.set_translation mi =<< toLowLevel (V3 0 0 (-5))

  return mi

laserMat :: IO GodotSpatialMaterial
laserMat = do
  mat <- unsafeInstance GodotSpatialMaterial "SpatialMaterial"
  clr <- toLowLevel $ opaque $ sRGB24read "8539ff" :: IO GodotColor
  G.set_albedo mat clr
  G.set_feature mat FEATURE_EMISSION True
  G.set_emission mat clr
  return mat
