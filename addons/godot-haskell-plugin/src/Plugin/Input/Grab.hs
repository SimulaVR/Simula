{-# LANGUAGE LambdaCase #-}
module Plugin.Input.Grab where

import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Plugin.Imports
import           Plugin.SimulaController
import           Plugin.WestonSurfaceSprite


data GrabState
  = NoGrab
  | Manipulating (GodotSimulaController, GodotWestonSurfaceSprite)
  | ManipulatingDual
      (GodotSimulaController, GodotWestonSurfaceSprite)
      (GodotSimulaController, GodotWestonSurfaceSprite)
  | Resizing (GodotSimulaController, GodotSimulaController) GodotWestonSurfaceSprite Float -- dist


processGrabEvent
  :: GodotSimulaController
  -> Maybe GodotWestonSurfaceSprite
  -> Bool
  -> GrabState
  -> IO GrabState
processGrabEvent gsc maybeWindow pressed = \case
  NoGrab
    | pressed -> return $ case maybeWindow of
      Just window -> Manipulating (gsc, window)
      Nothing     -> NoGrab
    | otherwise -> return NoGrab
  Manipulating ct1@(gsc1, curWindow)
    | pressed -> case maybeWindow of
      Just window
        | gsc == gsc1 -> NoGrab <$ putStrLn
          "Tried to press with the same controller without releasing"
        | window == curWindow -> startResize gsc1 gsc window
        | -- Second controller, same window
          otherwise -> return $ ManipulatingDual ct1 (gsc, window)
      Nothing -> return NoGrab
    | otherwise -> return NoGrab
  ManipulatingDual ct1@(gsc1, _) ct2@(gsc2, _)
    | not pressed && gsc == gsc2 -> return $ Manipulating ct1
    | not pressed && gsc == gsc1 -> return $ Manipulating ct2
    | otherwise -> NoGrab <$ putStrLn "Invalid: Input from a third controller"
  Resizing (gsc1, gsc2) w _
    | not pressed && gsc == gsc2 -> return $ Manipulating (gsc1, w)
    | not pressed && gsc == gsc1 -> return $ Manipulating (gsc2, w)
    | otherwise -> NoGrab <$ putStrLn "Invalid: Input from a third controller"
 where
  startResize gsc1 gsc2 curWindow = do
    -- Position of this controller
    pos1 <- G.get_global_transform gsc1 >>= Api.godot_transform_get_origin
    -- Position of other controller
    pos2 <- G.get_global_transform gsc2 >>= Api.godot_transform_get_origin

    dist <- realToFrac <$> Api.godot_vector3_distance_to pos1 pos2
    return $ Resizing (gsc1, gsc2) curWindow dist

handleState :: GrabState -> IO GrabState
handleState = \case
  Resizing (ct1, ct2) window origDistance -> do
    newpos1 <- G.get_global_transform ct1 >>= Api.godot_transform_get_origin
    newpos2 <- G.get_global_transform ct2 >>= Api.godot_transform_get_origin
    dist    <- realToFrac <$> Api.godot_vector3_distance_to newpos1 newpos2

    let scale = dist / origDistance
    toLowLevel (V3 scale scale scale) >>= G.scale_object_local window

    return $ Resizing (ct1, ct2) window dist

  state -> return state
