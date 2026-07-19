module Plugin.Debug.DamagedRegionTypes where

import Control.Exception (SomeException, try)
import Godot.Api.Auto (GodotTexture)
import Godot.Gdnative.Types
import System.Directory (Permissions, doesDirectoryExist, getPermissions, writable)
import System.Environment (lookupEnv)
import System.IO.Unsafe

import Plugin.Debug.HudTypes

debugDamagedRegionsEnvValue :: Maybe String
debugDamagedRegionsEnvValue = unsafePerformIO $
  lookupEnv "SIMULA_DEBUG_DAMAGED_REGIONS"
{-# NOINLINE debugDamagedRegionsEnvValue #-}

debugDamagedRegionsEnabled :: IO Bool
debugDamagedRegionsEnabled =
  debugHudModeActive DebugHudDamagedRegions

debugDamagedRegionsExportDirectory :: Maybe FilePath
debugDamagedRegionsExportDirectory = unsafePerformIO $ do
  simulaDebugDamagedRegions <- lookupEnv "SIMULA_DEBUG_DAMAGED_REGIONS"
  case simulaDebugDamagedRegions of
    Just path | path /= "1" && path /= "0" && path /= "" -> do
      directoryExists <- doesDirectoryExist path
      if directoryExists
        then do
          permissionsResult <- try (getPermissions path) :: IO (Either SomeException Permissions)
          if either (const False) writable permissionsResult
            then return (Just path)
            else do
              putStrLn $ "SIMULA_DEBUG_DAMAGED_REGIONS export path is not writable, skipping PNG exports: " ++ path
              return Nothing
        else do
          putStrLn $ "SIMULA_DEBUG_DAMAGED_REGIONS export path is not an existing directory, skipping PNG exports: " ++ path
          return Nothing
    _ -> return Nothing
{-# NOINLINE debugDamagedRegionsExportDirectory #-}

-- Pixel height allocated to one row of damaged region HUD thumbnails
debugDamagedRegionThumbnailRowHeight :: Int
debugDamagedRegionThumbnailRowHeight = 320

-- Horizontal pixel space between damaged region thumbnails
debugDamagedRegionThumbnailGridGap :: Int
debugDamagedRegionThumbnailGridGap = 12

-- In GSVS-relative coordinates.
type DebugDamagedRegionRect = (Float, Float, Float, Float)

data SnapshotTextureSourceGeometry =
  SnapshotTextureSourceGeometry
    { snapshotTextureSourceOffsetRight :: Float
    , snapshotTextureSourceOffsetDown  :: Float
    , snapshotTextureSourceWidth       :: Float
    , snapshotTextureSourceHeight      :: Float
    }

snapshotTextureSourceGeometryTuple :: SnapshotTextureSourceGeometry -> (Float, Float, Float, Float)
snapshotTextureSourceGeometryTuple
  (SnapshotTextureSourceGeometry right down width height) =
    (right, down, width, height)

-- Damaged region locations meant to be displayed directly over a gsvs
data DebugDamagedRegionOverlay = DebugDamagedRegionOverlay
  { ddroFrame :: Integer
  , ddroRects :: [DebugDamagedRegionRect]
  }

-- A "snapshot" refers to a captured texture image of the GSVS surface when the
-- damage event occurs along with its damage rect metadata. This is used for
-- thumbnail previews in the gsvs HUD.
data DebugDamagedRegionSnapshot = DebugDamagedRegionSnapshot
  { ddrsEventIndex :: Int
  , ddrsFrame :: Integer
  , ddrsRects :: [DebugDamagedRegionRect]
  , ddrsTexture :: Maybe GodotTexture
  }

-- Follows Godot Rect2 convention.
-- Carves out the bounds of the entire damaged-region section of the GSVS HUD.
data DebugHudDamagedRegionThumbnailsArea =
  DebugHudDamagedRegionThumbnailsArea
    { debugHudDamagedRegionThumbnailsAreaLeft :: Float
    , debugHudDamagedRegionThumbnailsAreaTop :: Float
    , debugHudDamagedRegionThumbnailsAreaWidth :: Float
    , debugHudDamagedRegionThumbnailsAreaHeight :: Float
    }

-- How many frames the damaged-region rectangles stay visible directly over the GSVS
debugDamagedRegionOverlayFrames :: Integer
debugDamagedRegionOverlayFrames = 30

-- How many frames must elapse before a duplicate damage event will record/display an additional thumbnail in the gsvs HUD
debugDamagedRegionDuplicateThumbnailSuppressionFrames :: Integer
debugDamagedRegionDuplicateThumbnailSuppressionFrames = 0 -- Set at 0 for now, so that we see literally every damage event in the HUD

-- How many damage events to keep history of and display in the HUD
debugDamagedRegionHistoryMax :: Int
debugDamagedRegionHistoryMax = 12

debugDamagedRegionThumbnailsPerRow :: Int
debugDamagedRegionThumbnailsPerRow = 3
