{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Plugin.Simula (GodotSimula(..)) where

import           Plugin.Imports
import           Data.Maybe

import           Plugin.Input
import           Plugin.Input.Grab
import           Plugin.SimulaController
import           Plugin.SimulaViewSprite
import           Plugin.VR
import           Plugin.Types
import           Plugin.PancakeCamera

import           Godot.Core.GodotVisualServer as G
import           Godot.Core.GodotGlobalConstants as G
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Godot.Gdnative.Types
import           Godot.Api
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G
import           Godot.Internal.Dispatch                  ( (:<)
                                                          , safeCast
                                                          )
import           Godot.Gdnative.Internal                  ( GodotNodePath
                                                          , GodotObject
                                                          )

import           System.Environment

data GodotSimula = GodotSimula
  { _sObj      :: GodotObject
  , _sGrabState :: TVar GrabState
  }

instance NativeScript GodotSimula where
  -- className = "Simula"
  classInit node  = GodotSimula (safeCast node)
    <$> newTVarIO NoGrab

  -- classExtends = "Node"
  classMethods =
    [ func NoRPC "_ready" (catchGodot Plugin.Simula.ready)
    , func NoRPC "_process" (catchGodot Plugin.Simula.process)
    , func NoRPC "on_button_signal" (catchGodot Plugin.Simula.on_button_signal)
    ]
  classSignals = []

instance HasBaseClass GodotSimula where
  type BaseClass GodotSimula = GodotNode
  super (GodotSimula obj _) = GodotNode obj


ready :: GodotSimula -> [GodotVariant] -> IO ()
ready self _ = do
  -- OpenHMD is unfortunately not yet a working substitute for OpenVR
  -- https://github.com/SimulaVR/Simula/issues/72

  gssSpatial <- addSimulaServerNode :: IO GodotSpatial
  maybeGSS <- asNativeScript (safeCast gssSpatial) :: IO (Maybe GodotSimulaServer)
  xrRuntimeJson <- lookupEnv "XR_RUNTIME_JSON"
  openBackend <- case (maybeGSS, xrRuntimeJson) of
    (Just gss, Nothing) -> do gssConf <- readTVarIO (gss ^. gssConfiguration)
                              let backend = _backend gssConf :: String
                              case backend of
                                "OpenXR" -> return openXR
                                _        -> do putStrLn "Unable to parse backend; defaulting to OpenXR"
                                               return openXR
    (Just gss, _) -> return openXR
    (Nothing, _) -> do return openXR

  debugModeMaybe <- lookupEnv "DEBUG"
  rrModeMaybe <- lookupEnv "RUNNING_UNDER_RR"
  case (rrModeMaybe, debugModeMaybe) of
    (Just rrModeVal, _)  -> putStrLn "RUNNING_UNDER_RR detected: not launching VR"
    (_, Just debugModeVal)  -> putStrLn "DEBUG mode detected: not launching VR"
    _ ->
      do openBackend >>= initVR (safeCast self) >>= \case
                InitVRSuccess -> do
                  putStrLn "InitVRSuccess"
                  vrViewport <- unsafeInstance GodotViewport "Viewport"

                  G.set_name vrViewport =<< toLowLevel "VRViewport"
                  G.set_update_mode vrViewport 3 -- UPDATE_ALWAYS
                  G.set_use_arvr vrViewport True
                  vrViewportSize <- toLowLevel (V2 100 100) :: IO GodotVector2 -- Godot requires us to set a default size
                  G.set_size vrViewport vrViewportSize

                  G.add_child self (safeCast vrViewport) True

                  orig <- unsafeInstance GodotARVROrigin "ARVROrigin"
                  G.add_child vrViewport (safeCast orig) True

                  -- Add the HMD as a child of the origin node
                  hmd <- unsafeInstance GodotARVRCamera "ARVRCamera"
                  G.add_child orig (safeCast hmd) True

                  -- Add two controllers and connect their button presses to the Simula
                  -- node.
                  let addCt = addSimulaController orig
                  addCt "LeftController" 1 >>= connectController
                  addCt "RightController" 2 >>= connectController
                  return ()

                InitVRFailed  -> putStrLn "InitVRFailed"

  gpcObj <- "res://addons/godot-haskell-plugin/PancakeCamera.gdns"
    & newNS' [] :: IO GodotObject
  maybeGPC <- asNativeScript gpcObj :: IO (Maybe GodotPancakeCamera)
  let gpc = Data.Maybe.fromJust maybeGPC
  G.set_current gpc True
  G.add_child self (safeCast gpc) True

  return ()
 where
  -- Helper function for black texture debugging.
  --   From the internet:
  --   var img = Image()
  --   var tex = ImageTexture.new()
  --   img.load("image.png")
  --   tex.create_from_image(img)
  getTextureFromURL :: String -> IO (GodotTexture)
  getTextureFromURL urlStr = do
    -- instance new types
    godotImage <- unsafeInstance GodotImage "Image" :: IO GodotImage
    godotImageTexture <- unsafeInstance GodotImageTexture "ImageTexture"

    -- Get image from URL
    pngUrl <- toLowLevel (pack urlStr) :: IO GodotString
    exitCode <- G.load godotImageTexture pngUrl -- load :: GodotImageTexture -> GodotString -> IO Int
    -- Load image into texture
    G.create_from_image godotImageTexture godotImage G.TEXTURE_FLAGS_DEFAULT
    return (safeCast godotImageTexture) -- NOTE: This [probably] leaks godotImage?


  addSimulaServerNode :: IO GodotSpatial
  addSimulaServerNode = do
    gss <- "res://addons/godot-haskell-plugin/SimulaServer.gdns"
      & newNS'' GodotSpatial "Spatial" []

    G.set_name gss =<< toLowLevel "SimulaServer"
    --G.add_child self (asObj gss) True
    G.add_child self ((safeCast gss) :: GodotNode)  True
    -- G.print_tree ((safeCast gss) :: GodotNode) -- Print tree for debugging
    -- addDummySprite3D gss -- Test
    return gss

  connectController :: GodotSimulaController -> IO ()
  connectController ct = do
    -- putStrLn "connectController"
    argsPressed <- Api.godot_array_new
    ctA <- toLowLevel $ toVariant $ asObj ct :: IO GodotVariant
    ctB <- toLowLevel $ toVariant $ asObj ct :: IO GodotVariant
    trueGV <- toLowLevel $ toVariant True :: IO GodotVariant
    falseGV <- toLowLevel $ toVariant False :: IO GodotVariant

    Api.godot_array_append argsPressed ctA
    Api.godot_array_append argsPressed trueGV

    argsReleased <- Api.godot_array_new
    Api.godot_array_append argsReleased ctB
    Api.godot_array_append argsReleased falseGV

    btnSignal   <- toLowLevel "on_button_signal"
    btnPressed  <- toLowLevel "button_pressed"
    btnReleased <- toLowLevel "button_release"

    G.connect ct btnPressed (safeCast self) btnSignal argsPressed 0
    G.connect ct btnReleased (safeCast self) btnSignal argsReleased 0

    mapM_ Api.godot_variant_destroy [ctA, ctB, trueGV, falseGV]
    mapM_ Api.godot_string_destroy [btnSignal, btnPressed, btnReleased]
    mapM_ Api.godot_array_destroy [argsPressed]

    return ()


on_button_signal :: GodotSimula -> [GodotVariant] -> IO ()
on_button_signal self [buttonVar, controllerVar, pressedVar] = do
  -- putStrLn "on_button_signal in Simula.hs"
  button <- fromGodotVariant buttonVar
  controllerObj <- fromGodotVariant controllerVar
  maybeController <- asNativeScript controllerObj -- tryObjectCast controllerObj
  let controller = Data.Maybe.fromJust maybeController
  --Just controller <- asNativeScript controllerObj -- tryObjectCast controllerObj
  pressed <- fromGodotVariant pressedVar
  onButton self controller button pressed
  return ()


onButton :: GodotSimula -> GodotSimulaController -> Int -> Bool -> IO ()
onButton self gsc button pressed = do
  -- putStrLn "onButton in Simula.hs"
  case (button, pressed) of
    (OVR_Button_Grip, False) -> -- Release grabbed
      readTVarIO gst
        >>= processGrabEvent gsc Nothing pressed
        >>= atomically
        .   writeTVar gst

    _ -> do
      let rc = _gscRayCast gsc
      G.force_raycast_update rc
      whenM (G.is_colliding rc) $ do
        maybeSprite <- G.get_collider rc >>= asNativeScript :: IO (Maybe GodotSimulaViewSprite) --fromNativeScript
        -- let sprite = Data.Maybe.fromJust maybeSprite
        maybe (return ()) (onSpriteInput rc) maybeSprite
          -- >>= maybe (return ()) (onSpriteInput rc)
 where
  gst = _sGrabState self
  onSpriteInput rc sprite =
    G.get_collision_point rc >>= case button of
      OVR_Button_Trigger -> processClickEvent sprite (Button pressed G.BUTTON_LEFT)
      OVR_Button_AppMenu -> processClickEvent sprite (Button pressed G.BUTTON_RIGHT)
      OVR_Button_Grip    -> const $
        readTVarIO gst
          >>= processGrabEvent gsc (Just sprite) pressed
          >>= atomically
          .   writeTVar gst
      _                  -> const $ return ()


process :: GodotSimula -> [GodotVariant] -> IO ()
process self _ = do
  -- putStrLn "process in Simula.hs"
  let gst = _sGrabState self
  atomically (readTVar gst)
    >>= handleState
    >>= atomically . writeTVar gst

  return ()
