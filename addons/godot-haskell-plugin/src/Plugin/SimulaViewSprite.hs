
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.SimulaViewSprite where

import Control.Exception
import Data.Proxy

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Core.GodotVisualServer as G
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody    as RigidBody
import           Godot.Gdnative.Internal.Api
import qualified Godot.Methods                as G
import qualified Godot.Gdnative.Internal.Api  as Api
import           Godot.Nativescript

import Plugin.SimulaCanvasItem
import Plugin.Types
import Data.Maybe
import Data.Either

-- import           Data.Vector.V2

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Control.Lens                hiding (Context)

import Data.Typeable

import qualified Data.Map.Strict as M

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance NativeScript GodotSimulaViewSprite where
  className = "SimulaViewSprite"
  classInit obj =
    do putStrLn "SimulaViewSprite Constructor called"
       -- putStrLn $ "show $ Proxy @(BaseClass a): " ++ (show $ Proxy @(BaseClass GodotSimulaViewSprite))
       GodotSimulaViewSprite (safeCast obj)
                      <$> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar True)
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      -- <*> atomically (newTVar False)
  -- classExtends = "RigidBody"
  classMethods =
    [ func NoRPC "_input_event" inputEvent
    , func NoRPC "_ready" ready

    , func NoRPC "_handle_destroy" _handle_destroy -- Connected in SimulaServer.hs
    , func NoRPC "_handle_map" _handle_map         -- Connected in SimulaServer.hs
    , func NoRPC "_process" Plugin.SimulaViewSprite._process
    , func NoRPC "_handle_unmap" _handle_unmap     -- Connected in SimulaServer.hs
    ]

  -- Test:
  classSignals = [ signal "map" [("gsvs", GodotVariantTypeObject)]
                 , signal "unmap" [("gsvs", GodotVariantTypeObject)]
                 ]

-- | Updates the GodotSimulaViewSprite state (including updating its texture).
-- | Intended to be called every frame.
updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  -- putStrLn "updateSimulaViewSprite"

  -- Update sprite texture; doesn't yet include popups or other subsurfaces.
  -- useViewportToDrawParentSurface gsvs
  -- drawParentWlrSurfaceTextureOntoSprite gsvs

  adjustDimensions gsvs 4096 4096

  useSimulaCanvasItemToDrawSubsurfaces gsvs
    -- useViewportToDrawParentSurface gsvs -- Causes _draw() error
    -- drawSurfacesOnSprite gsvs

  -- Set extents
  setExtents gsvs

  -- whenM (spriteShouldMove gsvs) $ do
  --   atomically $ writeTVar (_gsvsShouldMove gsvs) False
  --   moveToUnoccupied gsvs

  where adjustDimensions :: GodotSimulaViewSprite -> Int -> Int -> IO ()
        adjustDimensions gsvs w h = do
          gsci <- readTVarIO (gsvs ^. gsvsSimulaCanvasItem)
          renderTarget <- readTVarIO (gsci ^. gsciViewport)
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          wlrSurface <- getWlrSurface eitherSurface
          dimensions@(originalWidth, originalHeight) <- getBufferDimensions wlrSurface
          -- role <- case eitherSurface of
          --   Left wlrXdgSurface -> toLowLevel "" :: IO GodotString
          --   Right wlrXWaylandSurface -> G.get_role wlrXWaylandSurface

          let d'@(w',h') = if (originalWidth > 450 || originalHeight > 450)
                then (w,h)
                else (originalWidth, originalHeight)

          v <- toLowLevel (V2 (fromIntegral w') (fromIntegral h'))

          case eitherSurface of
            Left wlrXdgSurface -> return ()-- G.set_size wlrXdgSurface v
            Right wlrXWaylandSurface -> do G.set_size wlrXWaylandSurface v
                                           G.set_maximized wlrXWaylandSurface True

          pixelDimensionsOfWlrSurface <- toGodotVector2 d'
          G.set_size renderTarget pixelDimensionsOfWlrSurface

        -- As the name makes clear, this function *only* draws the parent WlrSurface
        -- onto a GodotSimulaViewSprite's Sprite3D field. It doesn't include popups or
        -- any other subsurfaces. This is a temporary hack that needs fixed.
        drawParentWlrSurfaceTextureOntoSprite :: GodotSimulaViewSprite -> IO ()
        drawParentWlrSurfaceTextureOntoSprite gsvs = do
          -- Get state
          sprite3D <- readTVarIO (gsvs ^. gsvsSprite)
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
          wlrSurface <- case wlrEitherSurface of
                                Left wlrXdgSurface       -> G.get_wlr_surface wlrXdgSurface
                                Right wlrXWaylandSurface -> do
                                  -- TODO: Complete this experiment
                                  -- children <- G.get_children wlrXWaylandSurface
                                  -- size' <- Api.godot_array_size children
                                  -- putStrLn $ "# of WlrXWayland Surfaces: " ++ (show size')
                                  G.get_wlr_surface wlrXWaylandSurface
          parentWlrTexture <- G.get_texture wlrSurface


          -- saveTextureToDisk wlrXdgSurface parentWlrTexture -- Causes crash
          -- Set Sprite3D texture
         
          -- Attempt to force maximize windows doesn't work:
          -- wlrXdgSurfaceToplevel <- G.get_xdg_toplevel wlrXdgSurface
          -- G.set_maximized wlrXdgSurfaceToplevel True -- Doesn't seem to work
          rid <- G.get_rid parentWlrTexture
          -- rid_canvas <- G.get_canvas self
          -- rid_canvas_item <- G.get_canvas self
          visualServer <- getSingleton GodotVisualServer "VisualServer"
          -- Enable everything but mipmapping (since this causes old textures get to interpolated
          -- with updated textures when far enough from the user).
 --         G.texture_set_flags visualServer rid 15
          -- G.texture_set_flags visualServer rid_canvas 6
          -- G.texture_set_flags visualServer rid_canvas_item 6
          G.set_texture sprite3D parentWlrTexture

          -- Tell client surface it should start rendering the next frame
          G.send_frame_done wlrSurface

        setExtents :: GodotSimulaViewSprite -> IO ()
        setExtents gsvs = do
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = simulaView ^. svWlrEitherSurface
          -- Get state
          sprite <- atomically $ readTVar (_gsvsSprite gsvs)
          aabb <- G.get_aabb sprite
          size <- godot_aabb_get_size aabb
          shape <- atomically $ readTVar (_gsvsShape gsvs)

          -- Compute new extents
          size' <- godot_vector3_operator_divide_scalar size 2
          (V3 x y z)  <- fromLowLevel size'
          -- size2d  <- toLowLevel (V2 x y) :: IO GodotVector2
          -- size2d' <- toLowLevel (V2 500 500) :: IO GodotVector2
          --G.set_size wlrXWaylandSurface size2d'

          -- Set extents
          G.set_extents shape size'

        spriteShouldMove :: GodotSimulaViewSprite -> IO Bool
        spriteShouldMove gsvs = do
          en <- atomically $ readTVar (_gsvsShouldMove gsvs)
          if en then do
            -- putStrLn "spriteShouldMove"
            sprite <- atomically $ readTVar (_gsvsSprite gsvs)
            aabb <- G.get_aabb sprite
            size <- godot_aabb_get_size aabb
            vsize <- fromLowLevel size
            return (vsize > 0)
            else return False

        -- TODO: check the origin plane?
        moveToUnoccupied :: GodotSimulaViewSprite -> IO ()
        moveToUnoccupied gsvs = do
          -- putStrLn "moveToUnoccupied"
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

ready :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
ready self _ = do
  -- putStrLn "ready in SimulaViewSprite.hs"
  G.set_mode self RigidBody.MODE_KINEMATIC
  return ()

inputEvent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
inputEvent self [_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] = do
  ev <- fromGodotVariant evObj
  clickPos <- fromGodotVariant clickPosObj
  processInputEvent self ev clickPos
  godot_object_destroy ev
  return ()

-- Needs more descriptive type constructor
data InputEventType
  = Motion
  | Button Bool -- (GodotIsPressed Bool)       ?
           Int  -- (GodotButtonDescriptor Int) ?

-- | Handles mouse (i.e., non-VR controller) events in pancake mode.
processInputEvent :: GodotSimulaViewSprite -> GodotObject -> GodotVector3 -> IO ()
processInputEvent gsvs ev clickPos = do
  -- putStrLn "processInputEvent"
  whenM (ev `isClass` "InputEventMouseMotion") $ processClickEvent gsvs Motion clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processClickEvent gsvs (Button pressed button) clickPos
    return ()

-- | This function used in `_on_WlrXdgShell_new_surface` (where we have access
-- | to GodotSimulaServer + GodotWlrXdgSurface).
newGodotSimulaViewSprite :: GodotSimulaServer -> SimulaView -> IO (GodotSimulaViewSprite)
newGodotSimulaViewSprite gss simulaView = do
  gsvsObj <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & newNS' [] :: IO GodotObject
  maybeGSVS <- asNativeScript gsvsObj :: IO (Maybe GodotSimulaViewSprite)
  let gsvs = Data.Maybe.fromJust maybeGSVS

  godotSprite3D <- unsafeInstance GodotSprite3D "Sprite3D"
  G.set_pixel_size godotSprite3D 0.001
  G.add_child gsvs (safeCast godotSprite3D) True

  shm <- load GodotShaderMaterial "ShaderMaterial" "res://addons/godot-haskell-plugin/TextShader.tres"
  case shm of
    Just shm -> G.set_material_override godotSprite3D (safeCast shm)
    Nothing -> error "couldn't fetch shader, hard failing for debug purposes"

  -- HACK: Set transparency to False to ensure that textures never disappear
  -- G.set_draw_flag godotSprite3D 0 False -- https://github.com/godotengine/godot/blob/89bcfa4b364e1edc8e175f766b50d145864eb159/scene/3d/sprite_3d.h#L44:7

  godotBoxShape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gsvs (safeCast gsvs)
  G.shape_owner_add_shape gsvs ownerId (safeCast godotBoxShape)

  -- simulaView <- readTVarIO (gsvs ^. gsvsView)
  -- let eitherSurface = (simulaView ^. svWlrEitherSurface)
  -- wlrSurface <- getWlrSurface eitherSurface
  -- dimensions@(originalWidth, originalHeight) <- getBufferDimensions wlrSurface
  --  atomically $ writeTVar (gsvs ^. gsvsTargetSize) (SpriteDimensions (originalWidth, originalHeight))
  atomically $ writeTVar (gsvs ^. gsvsTargetSize) (SpriteDimensions (800, 800))

  -- atomically $ writeTVar (_gsvsObj       gss) gsObj'      -- :: GodotObject (filled in classInit)
  atomically $ writeTVar (_gsvsServer    gsvs) gss           -- :: TVar GodotSimulaServer
  -- atomically $ writeTVar (_gsvsShouldMoe gsvs) gsvsShouldMoe' -- :: TVar Bool   (filled in classInit)
  atomically $ writeTVar (_gsvsSprite    gsvs) godotSprite3D -- :: TVar GodotSprite3D
  atomically $ writeTVar (_gsvsShape     gsvs) godotBoxShape -- :: TVar GodotBoxShape
  atomically $ writeTVar (_gsvsView      gsvs) simulaView    -- :: TVar SimulaView
  atomically $ writeTVar (_gsvsCursorCoordinates gsvs) (SurfaceLocalCoordinates (0,0))

  -- Initialize and load render target into _gsvsViewport field
    -- let wlrXdgSurface = (simulaView ^. svWlrXdgSurface)
    -- renderTarget <- initializeRenderTarget wlrXdgSurface
    -- atomically $ writeTVar (_gsvsViewport gsvs) renderTarget
  G.set_process gsvs False

--  updateSimulaViewSprite gsvs -- Now we update everything

  return gsvs

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView)
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer) -- ^. gssWlrSeat)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)

  atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) (Just gsvs)
  atomically $ writeTVar (gss ^. gssActiveCursorGSVS) (Just gsvs)

  case wlrEitherSurface of
    Left wlrXdgSurface -> do wlrSurface  <- G.get_wlr_surface wlrXdgSurface
                             G.reference wlrSurface
                             toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
                             -- isGodotTypeNull wlrSurface
                             G.set_activated toplevel True
                             -- G.keyboard_notify_enter wlrSeat wlrSurface
                             pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
                             pointerNotifyFrame wlrSeat
    Right wlrXWaylandSurface -> do wlrSurface  <- G.get_wlr_surface wlrXWaylandSurface
                                   case (((unsafeCoerce wlrXWaylandSurface) == nullPtr), ((unsafeCoerce wlrSurface) == nullPtr)) of
                                     (False, False) -> do G.reference wlrSurface
                                                          return ()
                                                          -- isGodotTypeNull wlrSurface
                                                          G.set_activated wlrXWaylandSurface True
                                                          -- G.keyboard_notify_enter wlrSeat wlrSurface
                                                          pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
                                                          pointerNotifyFrame wlrSeat
                                     _ -> putStrLn $ "Unable to focus on sprite!"

-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> GodotVector3
                  -> IO ()
processClickEvent gsvs evt clickPos = do
  surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) <- getSurfaceLocalCoordinates gsvs clickPos
  processClickEvent' gsvs evt surfaceLocalCoords

processClickEvent' :: GodotSimulaViewSprite
                  -> InputEventType
                  -> SurfaceLocalCoordinates
                  -> IO ()
processClickEvent' gsvs evt surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) = do
  -- putStrLn "processClickEvent"
  -- Get state
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) <-
    case wlrEitherSurface of
      Right godotWlrXWaylandSurface -> do
        G.set_activated godotWlrXWaylandSurface True
        getXWaylandSubsurfaceAndCoords godotWlrXWaylandSurface surfaceLocalCoords
      Left godotWlrXdgSurface -> getXdgSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords
  -- -- Send events
  case evt of
    Motion                -> do pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                                pointerNotifyMotion wlrSeat subSurfaceLocalCoords
    Button pressed button -> do focus gsvs
                                G.keyboard_notify_enter wlrSeat godotWlrSurface
                                pointerNotifyButton wlrSeat evt

  pointerNotifyFrame wlrSeat


-- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
-- | TODO: This function just returns parent surface/coords. Fix!
-- | TODO: Use _xdg_surface_at*
getXdgSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
getXdgSubsurfaceAndCoords wlrXdgSurface (SurfaceLocalCoordinates (sx, sy)) = do
  -- BROKEN since G.surface_at is broken (which causes G.get_surface to be broken).
  -- wlrSurfaceAtResult   <- G.surface_at  wlrXdgSurface sx sy -- Not NULL itself, but the wlr_surface within this data structure is NULL due to ~wlr_xdg_surface_surface_at(wlr_xdg_surface, ..)~ being NULL (but not because wlr_xdg_surface is NULL).
  -- wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult
  -- ssx                  <- G.get_sub_x wlrSurfaceAtResult
  -- ssy                  <- G.get_sub_y wlrSurfaceAtResult
  -- let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
  -- return (wlrSurfaceSubSurface, ssCoordinates)
  wlrSurfaceParent <- G.get_wlr_surface wlrXdgSurface            -- hack!
  G.reference wlrSurfaceParent
  return (wlrSurfaceParent, SubSurfaceLocalCoordinates (sx, sy)) -- hack!

getXWaylandSubsurfaceAndCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
getXWaylandSubsurfaceAndCoords wlrXWaylandSurface (SurfaceLocalCoordinates (sx, sy)) = do
  -- wlrSurfaceAtResult <- G.surface_at wlrXWaylandSurface sx sy
  ret@(wlrSurface', SubSurfaceLocalCoordinates (subX, subY)) <- withGodot
    (G.surface_at wlrXWaylandSurface sx sy)
    (destroyMaybe . safeCast)
    (\wlrSurfaceAtResult -> do G.reference wlrSurfaceAtResult
                               subX <- G.get_sub_x wlrSurfaceAtResult
                               subY <- G.get_sub_y wlrSurfaceAtResult
                               wlrSurface' <- G.get_surface wlrSurfaceAtResult
                               return (wlrSurface', SubSurfaceLocalCoordinates (subX, subY)))
  return ret

keyboardNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> IO ()
keyboardNotifyEnter wlrSeat wlrSurface = do
  G.keyboard_notify_enter wlrSeat wlrSurface

-- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
-- | need one since the GodotWlrSeat keeps internal track of what the currently
-- | active surface is.
pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (ssx, ssy)) = do
  G.pointer_notify_motion wlrSeat ssx ssy
  -- putStrLn $ "G.point_notify_motion: " ++ "(" ++ (show ssx) ++ ", " ++ (show ssy) ++ ")"

pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
pointerNotifyButton wlrSeat inputEventType = do
  case inputEventType of
      Motion -> return ()
      Button pressed buttonIndex -> pointerNotifyButton' pressed buttonIndex
  where pointerNotifyButton' pressed buttonIndex = do
          G.pointer_notify_button wlrSeat (fromIntegral buttonIndex) pressed
          -- putStrLn $ "G.pointer_notify_button: pressed/buttonIndex" ++ (show pressed) ++ "/" ++ (show buttonIndex)
          return ()

-- | Sends a frame event to the surface with pointer focus (apparently
-- | useful in particular for axis events); unclear if this is needed in VR but we
-- | use it regardless.
pointerNotifyFrame :: GodotWlrSeat -> IO ()
pointerNotifyFrame wlrSeat = do
  G.pointer_notify_frame wlrSeat

-- | Let wlroots know we have entered a new surface. We can safely call this
-- | over and over (wlroots checks if we've called it already for this surface
-- | and, if so, returns early.
pointerNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (ssx, ssy)) = do
  G.pointer_notify_enter wlrSeat wlrSurface ssx ssy -- Causing a crash

_handle_map :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_map self args = do
  simulaView <- readTVarIO (self ^. gsvsView)

  G.set_process self True
  G.set_process_input self True -- We do this in Godotston but not in original Simula
  emitSignal self "map" ([self] :: [GodotSimulaViewSprite])
  atomically $ writeTVar (simulaView ^. svMapped) True
  return ()

_handle_unmap :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_unmap self args = do
  simulaView <- readTVarIO (self ^. gsvsView)
  atomically $ writeTVar (simulaView ^. svMapped) False

-- Ensure that we de-focus the gsvs if it is active
  gss <- readTVarIO (self ^. gsvsServer)
  maybeGSVSFocused <- readTVarIO (gss ^. gssKeyboardFocusedSprite)
  case maybeGSVSFocused of
    Nothing -> return ()
    (Just gsvsFocused) -> do
      simulaViewFocused <- readTVarIO (gsvsFocused ^. gsvsView)
      if (simulaViewFocused == simulaView) then (atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) Nothing)
                                          else (return ())

  G.set_process self False
  emitSignal self "unmap" ([self] :: [GodotSimulaViewSprite])
  return ()

-- Passes control entirely to updateSimulaViewSprite.
_process :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_process self _ = do
  simulaView <- readTVarIO (self ^. gsvsView)
  mapped <- atomically $ readTVar (simulaView ^. svMapped)
  when mapped $ updateSimulaViewSprite self
  return ()


-- The original Simula didn't have a destroy handler at the Godot level.
-- 1. Was it leaking?
-- 2. Do Godot objects get deleted eventually anyway, even if we don't call queue_free?
-- 3. I'm assuming that `registerClass` passes a destructor for
--    GodotSimulaViewSprite that calls to `queue_free` can use.
_handle_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_destroy gsvs [gsvsGV] = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  gss <- readTVarIO (gsvs ^. gsvsServer)

  -- Destroy
  G.queue_free gsvs -- Queue the `gsvs` for destruction
  G.set_process gsvs False -- Remove the `simulaView ↦ gsvs` mapping from the gss
  atomically $ modifyTVar' (gss ^. gssViews) (M.delete simulaView)
    -- Old method of gsvs deletion from Simula; bring back if we face leakage issues:
  -- Api.godot_object_destroy (safeCast gsvs)
  deleteSurface eitherSurface

  where 
    deleteSurface eitherSurface = 
      case eitherSurface of
        (Left xdgSurface) -> destroyMaybe (safeCast xdgSurface)
        (Right xwaylandSurface) -> destroyMaybe (safeCast xwaylandSurface)

orientSpriteTowardsGaze :: GodotSimulaViewSprite -> IO ()
orientSpriteTowardsGaze gsvs = do 
  gss <- readTVarIO (gsvs ^. gsvsServer)
  upV3 <- toLowLevel (V3 0 1 0) :: IO GodotVector3
  rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
  targetV3 <- getARVRCameraOrPancakeCameraTransform gss >>= Api.godot_transform_get_origin -- void look_at ( Vector3 target, Vector3 up )
  G.look_at gsvs targetV3 upV3                      -- The negative z-axis of the gsvs looks at HMD
  -- G.rotate_object_local gsvs rotationAxisY 3.14159  -- The positive z-axis of the gsvs looks at HMD

  
-- | Push the gsvs by `dist` units along its object-local z-axis. Negative values of `dist`
-- | push the gsvs away from the user; positive values of `dist` push the gsvs
-- | towards the user.
moveSpriteAlongObjectZAxis :: GodotSimulaViewSprite -> Float -> IO ()
moveSpriteAlongObjectZAxis gsvs dist = do
  orientSpriteTowardsGaze gsvs
  pushBackVector <- toLowLevel (V3 0 0 dist) :: IO GodotVector3 -- For some reason we also have to shift the vector 0.5 units to the right
  G.translate_object_local gsvs pushBackVector
  return ()

-- Sets gssKeyboardGrabbedSprite to `Just (gsvs, dist)`
keyboardGrabInitiate :: GodotSimulaViewSprite -> IO ()
keyboardGrabInitiate gsvs = do
  gss <- readTVarIO $ (gsvs ^. gsvsServer)

  -- Compute dist
  orientSpriteTowardsGaze gsvs
  posGSVS <- (G.get_global_transform gsvs) >>= Api.godot_transform_get_origin
  hmdTransform <- getARVRCameraOrPancakeCameraTransform gss
  posHMD  <- Api.godot_transform_get_origin hmdTransform
  dist <- realToFrac <$> Api.godot_vector3_distance_to posGSVS posHMD
  -- Load state
  atomically $ writeTVar (gss ^. gssKeyboardGrabbedSprite) (Just (gsvs, (-dist)))

  return ()

-- Sets gssKeyboardGrabbedSprite to `Nothing`
keyboardGrabLetGo :: GodotSimulaViewSprite -> IO ()
keyboardGrabLetGo gsvs = do
  gss <- readTVarIO $ (gsvs ^. gsvsServer)

  -- Release state
  atomically $ writeTVar (gss ^. gssKeyboardGrabbedSprite) Nothing
  return ()

setInFrontOfUser :: GodotSimulaViewSprite -> Float -> IO ()
setInFrontOfUser gsvs zAxisDist = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
  pushBackVector <- toLowLevel (V3 0 0 zAxisDist) :: IO GodotVector3 -- For some reason we also have to shift the vector 0.5 units to the right
  hmdGlobalTransform <- getARVRCameraOrPancakeCameraTransform gss
  G.set_global_transform gsvs hmdGlobalTransform
  G.translate_object_local gsvs pushBackVector
  G.rotate_object_local gsvs rotationAxisY 3.14159 -- 180 degrees in radians
