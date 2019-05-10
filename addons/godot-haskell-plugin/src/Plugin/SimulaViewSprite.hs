
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.SimulaViewSprite where

import Debug.C

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Extra.Register
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import Plugin.Types
import Data.Maybe

-- import           Data.Vector.V2

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Control.Lens                hiding (Context)

import Data.Typeable

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
-- import           Graphics.Wayland.WlRoots.Compositor
import           Graphics.Wayland.WlRoots.Output
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.WlRoots.Backend
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.Render
-- import           Graphics.Wayland.WlRoots.Render.Color
-- import           Graphics.Wayland.WlRoots.OutputLayout
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Seat
-- import           Graphics.Wayland.WlRoots.Cursor
-- import           Graphics.Wayland.WlRoots.XCursorManager
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Input.Keyboard
-- import           Graphics.Wayland.WlRoots.Input.Pointer
-- import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.Input.Buttons
-- import           Graphics.Wayland.WlRoots.Box
import qualified Data.Map.Strict as M

C.initializeSimulaCtxAndIncludes

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance GodotClass GodotSimulaViewSprite where
  godotClassName = "SimulaViewSprite"

instance ClassExport GodotSimulaViewSprite where
  classInit obj =
    GodotSimulaViewSprite obj
                  <$> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar True)
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                  <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
  classExtends = "RigidBody"
  classMethods =
    [ GodotMethod NoRPC "_input_event" inputEvent
    , GodotMethod NoRPC "_ready" ready

    , GodotMethod NoRPC "_handle_destroy" _handle_destroy -- Connected in SimulaServer.hs
    , GodotMethod NoRPC "_handle_map" _handle_map         -- Connected in SimulaServer.hs
    , GodotMethod NoRPC "_handle_unmap" _handle_unmap     -- Connected in SimulaServer.hs
    , GodotMethod NoRPC "_process" Plugin.SimulaViewSprite._process
    ]

  -- Test:
  classSignals = [ signal "map" [("gsvs", GodotVariantTypeObject)]
                 , signal "unmap" [("gsvs", GodotVariantTypeObject)]
                 ]

instance HasBaseClass GodotSimulaViewSprite where
  type BaseClass GodotSimulaViewSprite = GodotRigidBody
  super (GodotSimulaViewSprite obj _ _ _ _ _ _) = GodotRigidBody obj

-- | Updates the GodotSimulaViewSprite state (including updating its texture).
-- | Intended to be called every frame.
updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  putStrLn "updateSimulaViewSprite"
  -- Update sprite texture; doesn't yet include popups or other subsurfaces.
  drawParentWlrSurfaceTextureOntoSprite gsvs
    -- useViewportToDrawParentSurface gsvs -- Causes _draw() error
    -- drawSurfacesOnSprite gsvs

  -- Set extents
  setExtents gsvs

  -- Move if needed
  whenM (spriteShouldMove gsvs) $ do
    atomically $ writeTVar (_gsvsShouldMove gsvs) False
    moveToUnoccupied gsvs

  -- Manually call _queue_update (doesn't fix black textures)
    -- sprite3D <- readTVarIO (gsvs ^. gsvsSprite)
    -- G._queue_update sprite3D -- ((safeCast gsvs) :: GodotSpriteBase3D )-- :: GodotSpriteBase3D IO ()
  return ()

  where
        -- Doesn't work; raises _draw() error.
        useViewportToDrawParentSurface :: GodotSimulaViewSprite -> IO ()
        useViewportToDrawParentSurface gsvs  = do
          putStrLn "drawParentSubsurfaceOnViewport"
          -- Get state
          sprite3D <- readTVarIO (gsvs ^. gsvsSprite)
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
          wlrSurface <- G.get_wlr_surface wlrXdgSurface -- G.get_wlr_surface :: GodotWlrXdgSurface -> IO (GodotWlrSurface)
          parentWlrTexture <- G.get_texture wlrSurface -- G.get_texture :: GodotWlrSurface -> IO (GodotTexture)

          -- Draw texture on Viewport; get its texture; set it to Sprite3D's texture; call G.send_frame_done
          let isNull = ((unsafeCoerce parentWlrTexture) == nullPtr)
          case isNull of
               True -> putStrLn "Texture is null!"
               False -> do renderTarget <- initializeRenderTarget wlrXdgSurface     -- Dumb, but we reset the renderTarget every frame to make sure the dimensions aren't (0,0)
                           atomically $ writeTVar (_gsvsViewport gsvs) renderTarget -- "
                           -- Get state
                           -- let zero = (V2 0 0) :: V2 Float
                           -- gv2 <- toLowLevel zero :: IO GodotVector2
                           -- return zero
                           renderPosition <- getCoordinatesFromCenter wlrXdgSurface 0 0 -- Surface coordinates are relative to the size of the GodotWlrXdgSurface; We draw at the top left.

                           textureToDraw <- G.get_texture wlrSurface :: IO GodotTexture
                           renderTarget <- readTVarIO (gsvs ^. gsvsViewport)

                             -- Send draw command
                           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
                           let nullTexture = Data.Maybe.fromJust ((fromVariant VariantNil) :: Maybe GodotTexture) :: GodotTexture
                           G.draw_texture ((unsafeCoerce renderTarget) :: GodotCanvasItem) textureToDraw renderPosition godotColor nullTexture
                           viewportTexture <- G.get_texture renderTarget
                           G.set_texture sprite3D (safeCast viewportTexture)
                           -- Tell the surface being drawn it can start to render its next frame
                           G.send_frame_done wlrSurface

        -- Doesn't work in Simula or Godotston
        saveTextureToDisk :: GodotWlrXdgSurface -> GodotTexture -> IO ()
        saveTextureToDisk wlrXdgSurface texture = do
            let isNull = ((unsafeCoerce texture) == nullPtr)
            case isNull of
                True -> putStrLn "Texture is null!"
                False -> do rid <- G.get_rid texture
                            isGodotTypeNull rid
                            visualServer <- getSingleton GodotVisualServer "VisualServer"
                            getBufferDimensions wlrXdgSurface
                            image <- G.texture_get_data visualServer rid 0

                            url <- toLowLevel (pack "res://SimulaTexture.png") :: IO GodotString
                            exitCode <- G.save_png image url
                            return ()

        -- As the name makes clear, this function *only* draws the parent WlrSurface
        -- onto a GodotSimulaViewSprite's Sprite3D field. It doesn't include popups or
        -- any other subsurfaces. This is a temporary hack that needs fixed.
        drawParentWlrSurfaceTextureOntoSprite :: GodotSimulaViewSprite -> IO ()
        drawParentWlrSurfaceTextureOntoSprite gsvs = do
          -- Get state
          sprite3D <- readTVarIO (gsvs ^. gsvsSprite)
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
          wlrSurface <- G.get_wlr_surface wlrXdgSurface -- G.get_wlr_surface :: GodotWlrXdgSurface -> IO (GodotWlrSurface)
          parentWlrTexture <- G.get_texture wlrSurface -- G.get_texture :: GodotWlrSurface -> IO (GodotTexture)

          -- saveTextureToDisk wlrXdgSurface parentWlrTexture -- Causes crash
          -- Set Sprite3D texture
          let isNull = ((unsafeCoerce parentWlrTexture) == nullPtr)
          case isNull of
               True -> putStrLn "Texture is null!"
               False -> G.set_texture sprite3D parentWlrTexture

          -- Failed texture jamming test:
            -- sampleTexture <- getTextureFromURL "res://sample_texture.png"
            -- G.set_texture sprite3D sampleTexture

          -- Failed texture extraction test:
            -- textureImage <- G.get_data sampleTexture -- parentWlrTexture :: IO (GodotImage) -- Causes crash
            -- url <- toLowLevel (pack "/home/george/Downloads/SimulaTexture.png") :: IO GodotString
            -- exitCode <- G.save_png textureImage url -- G.save_png GodotImage -> GodotString -> IO Int
            -- putStrLn $ "G.save_png exit code: " ++ (show exitCode)

          -- Tell client surface it should start rendering the next frame
          G.send_frame_done wlrSurface

        getTextureFromURL :: String -> IO (GodotTexture)
        getTextureFromURL urlStr = do
          -- instance new types
          godotImage <- unsafeInstance GodotImage "Image" :: IO GodotImage
          godotImageTexture <- unsafeInstance GodotImageTexture "ImageTexture"

          -- Get image from URL
          pngUrl <- toLowLevel (pack urlStr) :: IO GodotString
          exitCode <- G.load godotImageTexture pngUrl -- load :: GodotImageTexture -> GodotString -> IO Int

          -- Load image into texture
          G.create_from_image godotImageTexture godotImage 7 -- uses FLAGS_DEFAULT = 7

          return (safeCast godotImageTexture) -- NOTE: This [probably] leaks godotImage?

        setExtents :: GodotSimulaViewSprite -> IO ()
        setExtents gsvs = do
          -- Get state
          sprite <- atomically $ readTVar (_gsvsSprite gsvs)
          aabb <- G.get_aabb sprite
          size <- godot_aabb_get_size aabb
          shape <- atomically $ readTVar (_gsvsShape gsvs)

          -- Compute new extents
          size' <- godot_vector3_operator_divide_scalar size 2

          -- Set extents
          G.set_extents shape size'

        spriteShouldMove :: GodotSimulaViewSprite -> IO Bool
        spriteShouldMove gsvs = do
          en <- atomically $ readTVar (_gsvsShouldMove gsvs)
          if en then do
            putStrLn "spriteShouldMove"
            sprite <- atomically $ readTVar (_gsvsSprite gsvs)
            aabb <- G.get_aabb sprite
            size <- godot_aabb_get_size aabb
            vsize <- fromLowLevel size
            return (vsize > 0)
            else return False

        -- TODO: check the origin plane?
        moveToUnoccupied :: GodotSimulaViewSprite -> IO ()
        moveToUnoccupied gsvs = do
          putStrLn "moveToUnoccupied"
          gss <- readTVarIO (gsvs ^. gsvsServer)
          viewMap <- atomically $ readTVar (_gssViews gss)
          let otherGsvs = filter (\x -> asObj x /= asObj gsvs) $ M.elems viewMap

          extents <- forM otherGsvs $ \viewSprite -> do
            sprite <- atomically $ readTVar (gsvs ^. gsvsSprite) -- getSprite viewSprite
            aabb   <- G.get_transformed_aabb sprite
            size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
            pos    <- Api.godot_aabb_get_position aabb >>= fromLowLevel
            return (pos, size + pos)

          let minX = minimum $ 0 : map (view $ _1._x) extents
              maxX = maximum $ 0 :  map (view $ _2._x) extents
          sprite <- atomically $ readTVar (gsvs ^. gsvsSprite)
          aabb   <- G.get_aabb sprite
          size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
          let sizeX  = size ^. _x
              newPos =
                if abs minX < abs maxX
                then V3 (minX - sizeX/2) 0 0
                else V3 (maxX + sizeX/2) 0 0

          G.translate gsvs =<< toLowLevel newPos

        -- | Draws the given surface onto the GodotSimulaViewSprite's Viewport (treated
        -- | as a rendering target). Meant to be called for all of the
        -- | GodotSimulaViewSprite's subsurfaces, to create the texture to be applied for
        -- | the GodotSimulaViewSprite's Sprite3D.
        -- |
        -- | TODO: This function is missing a G.draw_texture call (we need a CanvasItem argument). Fix this.
        drawSubsurfaceOnViewport :: GodotSimulaViewSprite -- Contains the viewport
                                  -> GodotWlrSurface
                                  -> CInt
                                  -> CInt
                                  -> IO ()
        drawSubsurfaceOnViewport gsvs wlrSurface sx sy = do
          putStrLn "drawSubsurfaceOnViewport"
          -- Get state
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
          renderPosition <- getCoordinatesFromCenter wlrXdgSurface sx sy -- Surface coordinates are relative to the size of the GodotWlrXdgSurface
          textureToDraw <- G.get_texture wlrSurface :: IO GodotTexture
          renderTarget <- readTVarIO (gsvs ^. gsvsViewport)

            -- Send draw command
          godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1) :: IO GodotColor
          let nullTexture = Data.Maybe.fromJust ((fromVariant VariantNil) :: Maybe GodotTexture) :: GodotTexture
          -- G.draw_texture ((safeCast renderTarget) :: GodotCanvasItem) textureToDraw renderPosition godotColor nullTexture

          -- Tell the surface being drawn it can start to render its next frame
          G.send_frame_done wlrSurface
          -- NOTE: I'm omitting re-mutating gsvsViewport, G.draw_texture should merely
          -- mutate what it's pointing to anyway.

        -- | Convert (sx,sy) coordinates from "top-left" to "from-center" coordinate systems.
        getCoordinatesFromCenter :: GodotWlrXdgSurface -> CInt -> CInt -> IO GodotVector2
        getCoordinatesFromCenter wlrXdgSurface sx sy = do
          putStrLn "getCoordinatesFromCenter"
          (bufferWidth', bufferHeight')    <- getBufferDimensions wlrXdgSurface
          let (bufferWidth, bufferHeight)  = (fromIntegral bufferWidth', fromIntegral bufferHeight')
          let (fromTopLeftX, fromTopLeftY) = (fromIntegral sx, fromIntegral sy)
          let fromCenterX                  = -(bufferWidth/2) + fromTopLeftX
          let fromCenterY                  = -(-(bufferHeight/2) + fromTopLeftY)
          -- NOTE: In godotston fromCenterY is isn't negative, but since we set
          -- `G.render_target_v_flip viewport True` we can set this
          -- appropriately
          -- NOTE: We above assume that
          --    G.render_target_v_flip viewport True
          -- has been set.
          let v2 = (V2 fromCenterX fromCenterY) :: V2 Float
          gv2 <- toLowLevel v2 :: IO GodotVector2
          return gv2

        -- TODO: Implement this function
        drawSurfacesOnSprite :: GodotSimulaViewSprite -> IO ()
        drawSurfacesOnSprite gsvs = do
           putStrLn "drawSurfacesOnSprite"
           simulaView <- readTVarIO (gsvs ^. gsvsView)
           let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
           -- listOfWlrSurface <- getSubsurfaces wlrXdgSurface -- getSurfaces :: GodotWlrXdgSurface -> IO [(WlrSurface, sx, sy)]
           -- forM_ listOfWlrSurface
           --    (\(wlrSurface, sx, sy) -> do drawSubsurfaceOnViewport wlrSurface sx sy)
           --                                 G.send_frame_done wlrSurface
           -- viewportTexture <- ...
           -- G.set_texture sprite3D parentWlrTexture
           return ()

ready :: GFunc GodotSimulaViewSprite
ready self _ = do
  putStrLn "ready in SimulaViewSprite.hs"
  G.set_mode self RigidBody.MODE_KINEMATIC
  toLowLevel VariantNil

inputEvent :: GFunc GodotSimulaViewSprite
inputEvent self args = do
  putStrLn "inputEvent in SimulaViewSprite.hs"
  case toList args of
    [_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] ->  do
      ev <- fromGodotVariant evObj
      clickPos <- fromGodotVariant clickPosObj
      processInputEvent self ev clickPos
      godot_object_destroy ev
    _ -> putStrLn "expected 5 arguments in _input_event"
  toLowLevel VariantNil

-- Needs more descriptive type constructor
data InputEventType
  = Motion
  | Button Bool -- (GodotIsPressed Bool)       ?
           Int  -- (GodotButtonDescriptor Int) ?

-- | Handles mouse (i.e., non-VR controller) events in pancake mode.
processInputEvent :: GodotSimulaViewSprite -> GodotObject -> GodotVector3 -> IO ()
processInputEvent gsvs ev clickPos = do
  putStrLn "processInputEvent"
  whenM (ev `isClass` "InputEventMouseMotion") $ processClickEvent gsvs Motion clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processClickEvent gsvs (Button pressed button) clickPos

-- | This function used in `_on_WlrXdgShell_new_surface` (where we have access
-- | to GodotSimulaServer + GodotWlrXdgSurface).
newGodotSimulaViewSprite :: GodotSimulaServer -> SimulaView -> IO (GodotSimulaViewSprite)
newGodotSimulaViewSprite gss simulaView = do
  putStrLn "newGodotSimulaViewSprite"
  gsvs <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & newNS' []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr :: IO GodotSimulaViewSprite -- w/_gsvsObj populated + mempty TVars

  godotSprite3D <- unsafeInstance GodotSprite3D "Sprite3D"
  G.set_pixel_size godotSprite3D 0.001
  G.add_child gsvs (safeCast godotSprite3D) True
  G.set_flip_h godotSprite3D True

  godotBoxShape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gsvs (safeCast gsvs)
  G.shape_owner_add_shape gsvs ownerId (safeCast godotBoxShape)

  -- atomically $ writeTVar (_gsvsObj       gss) gsObj'      -- :: GodotObject (filled in classInit)
  atomically $ writeTVar (_gsvsServer    gsvs) gss           -- :: TVar GodotSimulaServer
  -- atomically $ writeTVar (_gsvsShouldMoe gsvs) gsvsShouldMoe' -- :: TVar Bool   (filled in classInit)
  atomically $ writeTVar (_gsvsSprite    gsvs) godotSprite3D -- :: TVar GodotSprite3D
  atomically $ writeTVar (_gsvsShape     gsvs) godotBoxShape -- :: TVar GodotBoxShape
  atomically $ writeTVar (_gsvsView      gsvs) simulaView    -- :: TVar SimulaView

  -- Initialize and load render target into _gsvsViewport field
  let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
  renderTarget <- initializeRenderTarget wlrXdgSurface
  atomically $ writeTVar (_gsvsViewport gsvs) renderTarget

  updateSimulaViewSprite gsvs -- Now we update everything

  return gsvs

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  putStrLn "focus in SimulaViewSprite.hs"
  -- Get state:
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView) 
  let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
  wlrSurface  <- G.get_wlr_surface wlrXdgSurface
  wlrSurface' <- asGodotVariant wlrSurface
  toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer) -- ^. gssWlrSeat)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)

  -- Make calls:
  G.set_activated toplevel True
  G.keyboard_notify_enter wlrSeat wlrSurface'

  return ()


-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
-- |
-- | TODO: Change this horifically named function.
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> GodotVector3
                  -> IO ()
processClickEvent gsvs evt clickPos = do
  putStrLn "processClickEvent"
  -- Get state
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let godotWlrXdgSurface = (simulaView ^. svWlrXdgSurface)


  -- Compute subsurface local coordinates at clickPos
  surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) <- getSurfaceLocalCoordinates
  (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) <- getSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords -- This function is a hack that only returns parent surface + its coordinates; needs fixing.

  -- -- Send events
  case evt of
    Motion                -> do pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                                pointerNotifyMotion wlrSeat subSurfaceLocalCoords
    Button pressed button ->    pointerNotifyButton wlrSeat evt

  pointerNotifyFrame wlrSeat -- No matter what, send a frame event to the surface with pointer focus
                             -- TODO: Is this necessary in VR?

  where
    getSurfaceLocalCoordinates :: IO (SurfaceLocalCoordinates)
    getSurfaceLocalCoordinates = do
      lpos <- G.to_local gsvs clickPos >>= fromLowLevel
      sprite <- atomically $ readTVar (_gsvsSprite gsvs)
      aabb <- G.get_aabb sprite
      size <- godot_aabb_get_size aabb >>= fromLowLevel
      let topleftPos =
            V2 (size ^. _x / 2 - lpos ^. _x) (size ^. _y / 2 - lpos ^. _y)
      let scaledPos = liftI2 (/) topleftPos (size ^. _xy)
      rect <- G.get_item_rect sprite
      recSize <- godot_rect2_get_size rect >>= fromLowLevel
      let coords = liftI2 (*) recSize scaledPos
      -- coords = surface coordinates in pixel with (0,0) at top left
      let sx = fromIntegral $ truncate (256 * coords ^. _x)
          sy = fromIntegral $ truncate (256 * coords ^. _y)
      return (SurfaceLocalCoordinates (sx, sy))

    -- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
    -- | TODO: This function just returns parent surface/coords. Fix!
    getSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
    getSubsurfaceAndCoords wlrXdgSurface (SurfaceLocalCoordinates (sx, sy)) = do
      -- BROKEN since G.surface_at is broken (which causes G.get_surface to be broken).
      -- wlrSurfaceAtResult   <- G.surface_at  wlrXdgSurface sx sy -- Not NULL itself, but the wlr_surface within this data structure is NULL due to ~wlr_xdg_surface_surface_at(wlr_xdg_surface, ..)~ being NULL (but not because wlr_xdg_surface is NULL).
      -- wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult
      -- ssx                  <- G.get_sub_x wlrSurfaceAtResult
      -- ssy                  <- G.get_sub_y wlrSurfaceAtResult
      -- let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
      -- return (wlrSurfaceSubSurface, ssCoordinates)
      wlrSurfaceParent <- G.get_wlr_surface wlrXdgSurface            -- hack!
      return (wlrSurfaceParent, SubSurfaceLocalCoordinates (sx, sy)) -- hack!

    -- | Let wlroots know we have entered a new surface. We can safely call this
    -- | over and over (wlroots checks if we've called it already for this surface
    -- | and, if so, returns early.
    pointerNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
    pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (ssx, ssy)) = do
      let maybeWlrSurfaceGV = (fromVariant ((toVariant wlrSurface) :: Variant 'GodotTy) :: Maybe GodotVariant)
      case maybeWlrSurfaceGV of
          Nothing -> putStrLn "Failed to convert GodotWlrSurface to GodotVariant!"
          Just wlrSurfaceGV -> G.pointer_notify_enter wlrSeat wlrSurfaceGV ssx ssy -- Causing a crash

    -- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
    -- | need one since the GodotWlrSeat keeps internal track of what the currently
    -- | active surface is.
    pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
    pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (ssx, ssy)) = do
      G.pointer_notify_motion wlrSeat ssx ssy

    pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
    pointerNotifyButton wlrSeat inputEventType = do
      case inputEventType of
          Motion -> return ()
          Button pressed buttonIndex -> pointerNotifyButton' pressed buttonIndex
      where pointerNotifyButton' pressed buttonIndex = do
              buttonIndexVariant <- toLowLevel (VariantInt buttonIndex) :: IO GodotVariant
              G.pointer_notify_button wlrSeat buttonIndexVariant pressed
              return ()

    -- | Sends a frame event to the surface with pointer focus (apparently
    -- | useful in particular for axis events); unclear if this is needed in VR but we
    -- | use it regardless.
    pointerNotifyFrame :: GodotWlrSeat -> IO ()
    pointerNotifyFrame wlrSeat = do
      G.pointer_notify_frame wlrSeat

_handle_map :: GFunc GodotSimulaViewSprite
_handle_map self args = do
  putStrLn "_handle_map"
  case toList args of
    [gsvsGV] ->  do
      G.set_process self True
      -- G.set_process_input self True -- We do this in Godotston but not in original Simula; deciding to not do it

      -- Am mirroring godotston here, but conceptually it's unclear to me what
      -- this is doing, and why it wouldn't just result in a _handle_map call
      -- loop that never terminates.
      emitSignal self "map" ([] :: [GodotSimulaViewSprite])
      putStrLn "Called _handle_map. Should only see this once per surface launch." -- <- Test
    _ -> putStrLn "Failed to get arguments in _handle_map"

  toLowLevel VariantNil

_handle_unmap :: GFunc GodotSimulaViewSprite
_handle_unmap self args = do
  putStrLn "_handle_unmap"
  case toList args of
    [gsvsGV] ->  do
      maybeGsvs <- variantToReg gsvsGV :: IO (Maybe GodotSimulaViewSprite)
      case maybeGsvs of
        Nothing -> putStrLn "Failed to cast gsvs in _handle_destroy!"
        (Just gsvs) -> G.set_process gsvs False
                      -- G.set_process_input gsvs False -- We do this in Godotston but not in original Simula; deciding to not do it

      -- Am mirroring godotston here, but conceptually it's unclear to me what
      -- this is doing, and why it wouldn't just result in a _handle_unmap call
      -- loop that never terminates.
      emitSignal self "unmap" ([] :: [GodotSimulaViewSprite])
      putStrLn "Called _handle_unmap. Should only see this once per surface launch." -- <- Test
  toLowLevel VariantNil

-- Passes control entirely to updateSimulaViewSprite.
_process :: GFunc GodotSimulaViewSprite
_process self args = do
  -- putStrLn "_process in SimulaViewSprite.hs"
  case toList args of
    [deltaGV] ->  do
      updateSimulaViewSprite self

  toLowLevel VariantNil


-- The original Simula didn't have a destroy handler at the Godot level.
-- 1. Was it leaking?
-- 2. Do Godot objects get deleted eventually anyway, even if we don't call queue_free?
-- 3. I'm assuming that `registerClass` passes a destructor for
--    GodotSimulaViewSprite that calls to `queue_free` can use.
_handle_destroy :: GFunc GodotSimulaViewSprite
_handle_destroy self args = do
  putStrLn "_handle_destroy"
  case toList args of
    [gsvsGV] ->  do
      -- Get state
      maybeGsvs <- variantToReg gsvsGV :: IO (Maybe GodotSimulaViewSprite)
      case maybeGsvs of
        Nothing -> putStrLn "Failed to cast gsvs in _handle_destroy!"
        (Just gsvs) -> do simulaView <- readTVarIO (gsvs ^. gsvsView)
                          gss <- readTVarIO (gsvs ^. gsvsServer)

                          -- Destroy
                          G.queue_free gsvs -- Queue the `gsvs` for destruction
                          G.set_process gsvs False -- Remove the `simulaView ↦ gsvs` mapping from the gss
                          atomically $ modifyTVar' (gss ^. gssViews) (M.delete simulaView)
                            -- Old method of gsvs deletion from Simula; bring back if we face leakage issues:
                            -- Api.godot_object_destroy (safeCast gsvs)

  toLowLevel VariantNil

 -- Our ideal rendering strategy: each GodotSimulaViewSprite has a
 -- GodotSprite3D, which expects a texture with our wlroots surface. In order to
 -- supply this wlroots texture, we have to make it.

 -- To do this, we start with the texture provided by our GodotWlrXdgSurface,
 -- and apply it it to a Viewport (used purely as a "rendering target") as a
 -- base, and then render each of the GodotWlrXdgSurface's subsurfaces over it
 -- (which contain popups, menus, etc). The final result will be a Viewport
 -- texture with all of the subsurfaces pancaked on top of each other, creating
 -- the illusion of one large wlroots surface. We finish by supplying this
 -- Viewport texture to our Sprite3D, creating the desired illusion of having a
 -- wlroots surface floating in 3-space.

 -- NOTE: You might ask, "why must we use a Viewport?" The answer is that this
 -- is the ritual we engage in to modify 2D textures in a 3D Godot game. As long
 -- as our Viewport isn't the Root Viewport, it is treated by Godot as a
 -- "rendering target" instead of a screen to render onto. For this reason we
 -- associate with each GodotSimulaViewSprite its own unique GodotViewport.


initializeRenderTarget :: GodotWlrXdgSurface -> IO (GodotViewport)
initializeRenderTarget wlrXdgSurface = do
  putStrLn "initializeRenderTarget"
  -- "When we are drawing to a Viewport that is not the Root, we call it a
  --  render target." -- Godot documentation"
  renderTarget <- unsafeInstance GodotViewport "Viewport"
  -- No need to add the Viewport to the SceneGraph since we plan to use it as a render target
    -- G.set_name viewport =<< toLowLevel "Viewport"
    -- G.add_child gsvs ((safeCast viewport) :: GodotObject) True

  G.set_disable_input renderTarget True -- Turns off input handling

  G.set_usage renderTarget 0 -- USAGE_2D = 0
  -- G.set_hdr renderTarget False -- Might be useful to disable HDR rendering for performance in the future (requires upgrading gdwlroots to GLES3)

  -- CLEAR_MODE_ALWAYS = 0
  -- CLEAR_MODE_NEVER = 1
  -- I think we need CLEAR_MODE_ALWAYS since, i.e., a popup dragging might cause a trail?
  G.set_clear_mode renderTarget 0

  -- Perhaps we should never update the render target, since we do so manually each frame?
  -- UPDATE_DISABLED = 0 — Do not update the render target.
  -- UPDATE_ONCE = 1 — Update the render target once, then switch to UPDATE_DISABLED.
  -- UPDATE_WHEN_VISIBLE = 2 — Update the render target only when it is visible. This is the default value.
  -- UPDATE_ALWAYS = 3 — Always update the render target. 
  G.set_update_mode renderTarget 3 -- Using UPDATE_ALWAYS for now

  -- "Note that due to the way OpenGL works, the resulting ViewportTexture is flipped vertically. You can use Image.flip_y on the result of Texture.get_data to flip it back[or you can also use set_vflip]:" -- Godot documentation
  G.set_vflip renderTarget True -- In tutorials this is set as True, but no reference to it in Godotston; will set to True for now

  -- We could alternatively set the size of the renderTarget via set_size_override [and set_size_override_stretch]
  dimensions@(width, height) <- getBufferDimensions wlrXdgSurface
  pixelDimensionsOfWlrXdgSurface <- toGodotVector2 dimensions

  -- Here I'm attempting to set the size of the viewport to the pixel dimensions
  -- of our wlrXdgSurface argument:
  G.set_size renderTarget pixelDimensionsOfWlrXdgSurface

  -- There is, however, an additional way to do this and I'm not sure which one
  -- is better/more idiomatic:
    -- G.set_size_override renderTarget True vector2
    -- G.set_size_override_stretch renderTarget True

  return renderTarget
  where
        -- | Used to supply GodotVector2 to
        -- |   G.set_size :: GodotViewport -> GodotVector2 -> IO ()
        toGodotVector2 :: (Int, Int) -> IO (GodotVector2)
        toGodotVector2 (width, height) = do
          let v2 = (V2 (fromIntegral width) (fromIntegral height))
          gv2 <- toLowLevel v2 :: IO (GodotVector2)
          return gv2

-- Possible TODO: Place types for different coordinate systems
-- newtype BufferDimensions = BufferDimensions (Int, Int)
-- data FromTopLeftCoordinates = FromTopLeftCoordinates (Int, Int)
-- data FromCenterCoordinates = FromCenterCoordinates (Int, Int)

getBufferDimensions :: GodotWlrXdgSurface -> IO (Int, Int)
getBufferDimensions wlrXdgSurface = do
  putStrLn "getBufferDimensions"
  wlrSurface <- G.get_wlr_surface wlrXdgSurface -- isNull: False
  wlrSurfaceState <- G.get_current_state wlrSurface -- isNull: False
  bufferWidth <- G.get_buffer_width wlrSurfaceState
  bufferHeight <- G.get_buffer_height wlrSurfaceState
  width <- G.get_width wlrSurfaceState
  height <-G.get_height wlrSurfaceState
  putStrLn $ "getBufferDimensions (buffer width/height): (" ++ (show bufferWidth) ++ "," ++ (show bufferHeight) ++ ")"
  putStrLn $ "getBufferDimensions (width/height): (" ++ (show width) ++ "," ++ (show height) ++ ")"
  return (bufferWidth, bufferHeight) -- G.set_size expects "the width and height of viewport" according to Godot documentation

getTextureFromRenderTarget :: GodotViewport -> IO (GodotTexture)
getTextureFromRenderTarget renderTarget = do
  putStrLn "getTextureFromRenderTarget"
  viewportTexture' <- G.get_texture renderTarget -- G.get_texture :: GodotViewport -> (IO GodotViewportTexture)
  let viewportTexture = (safeCast viewportTexture) :: GodotTexture -- GodotTexture :< GodotViewportTexture
  -- -- Retrieving an image
  -- G.get_data viewportTexture :: IO GodotImage -- requires viewportTexture to be cast as GodotTexture
  return viewportTexture
