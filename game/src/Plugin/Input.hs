{-# LANGUAGE LambdaCase, DataKinds, MultiParamTypeClasses #-}
module Plugin.Input where

import qualified Godot.Methods               as G

import           Plugin.Imports


data PointerEvent
  = Motion
  | Click Bool Int -- isPressed and buttonID

pointerEvent :: PointerEvent -> GodotVector3 -> GodotCollisionObject -> IO ()
pointerEvent evType pos window = do
  (funcName, funcArgs) <- case evType of
    Motion -> do
      funcName <- toLowLevel "motionEvent"
      return ( funcName
             , [ toVariant pos ]
             )

    Click isPressed buttonID -> do
      funcName <- toLowLevel "clickEvent"
      return ( funcName
             , [ toVariant isPressed
               , toVariant buttonID
               , toVariant pos
               ]
             )

  _ <- G.call window funcName funcArgs

  return ()

setInputHandled :: (GodotNode :< a) => a -> IO ()
setInputHandled self = do
  st <- G.get_tree (safeCast self :: GodotNode)
  G.set_input_as_handled st


pattern OVR_Button_Touchpad :: Int
pattern OVR_Button_Touchpad = 14

pattern OVR_Button_Trigger :: Int
pattern OVR_Button_Trigger = 15

pattern OVR_Button_AppMenu :: Int
pattern OVR_Button_AppMenu = 1

pattern OVR_Button_Grip :: Int
pattern OVR_Button_Grip = 2

