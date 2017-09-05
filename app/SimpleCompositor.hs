import Control.Exception
import Control.Lens
import Control.Concurrent.MVar
import Data.Typeable

import Simula.Compositor.Compositor
import Simula.Compositor.Wayland.Input
import Simula.Compositor.Weston
import Simula.Compositor.WindowManager
import Simula.Compositor.SceneGraph
import Simula.Compositor.Types
import Simula.Compositor.Utils
import Simula.WestonDesktop
import Simula.Weston
import Simula.WaylandServer
import Foreign
import Foreign.C
import Linear
import DBus
import DBus.Client

import Graphics.Rendering.OpenGL hiding (translate, scale, rotate)

pingFunc :: MethodCall -> IO Reply
pingFunc _ = return $ replyReturn []

sayHello :: String -> IO String
sayHello name = return ("Hello " ++ name ++ "!")

connectDbus :: IO Client
connectDbus = do
  dbus <- connectSession
  reply <- requestName dbus "com.SimulaHS.GenericCompositor" [nameDoNotQueue]
  case reply of
    NameExists ->
      putStrLn "Could not register name with DBus: it already exists" >> return dbus

    NameInQueue -> 
      putStrLn "Could not register name with DBus: would be queued or have been queued?" >> return dbus

    NameAlreadyOwner ->
      putStrLn "DBus name is already owned by us" >> return dbus 

    NamePrimaryOwner -> do
      export dbus "/example"
        [ method "com.SimulaHS.GenericCompositor" "Ping" (signature_ []) (signature_ []) pingFunc
        , autoMethod "com.SimulaHS.GenericCompositor" "Hello" sayHello
        ]
      putStrLn "Published DBus Features" >> return dbus

cleanupDbus :: Client -> IO ()
cleanupDbus dbus = do
  release <- releaseName dbus "com.SimulaHS.GenericCompositor"
  case release of
    NameReleased -> putStrLn "Released com.SimulaHS.GenericCompositor from DBus"
    NameNotOwner -> putStrLn "Attemped to release com.SimulaHS.GenericCompositor from DBus and we are not the owner"
    NameNonExistent -> putStrLn "Attemped to release com.SimulaHS.GenericCompositor from DBus and the name does not exist"        

mainBody :: Client -> IO ()
mainBody dbus = do
  seat <- newSimulaSeat
  let dpRot = axisAngle (V3 1 0 0) (radians (negate 25))
  let dpTf = translate (V3 0 0.8 1.25) !*! m33_to_m44 (fromQuaternion dpRot)
  rec -- order is important
    comp <- newSimulaCompositor scene disp
    Just glctx <- readMVar (comp ^. simulaCompositorGlContext)
    scene <- Scene <$> newBaseNode scene Nothing identity
           <*> newMVar 0 <*> newMVar 0
           <*> pure wm <*> newMVar (Some comp) <*> newMVar [] <*> newMVar Nothing
    disp <- newDisplay glctx (V2 1280 720) (V2 0.325 0.1) scene dpTf
    vp <- newViewPoint 0.01 100 disp disp (translate (V3 0 0 0.1)) (V4 0 0 1 1) (V3 0 0 0)
    modifyMVar' (disp ^. displayViewpoints) (vp:)
    modifyMVar' (scene ^. sceneDisplays) (disp:)

    wm <- newWindowManager scene seat
  startCompositor comp


main :: IO ()
main = do
  dbus <- connectDbus
  mainBody dbus
