{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Plugin (registerClasses) where

import           Control.Monad           (forM)
import qualified Data.Text               as T

import           Godot.Gdnative.Internal
import           Godot.Nativescript

import           Plugin.WestonSurfaceTexture
import           Plugin.Weston
import           Plugin.Types
import           Plugin.Util             (godotPrint)



type RegisterClassFunc a = GdnativeHandle -> (GodotObject -> IO a) -> IO ()


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg f = f desc classInit
  reg (registerClass' :: RegisterClassFunc GodotWestonSurfaceTexture)
  reg (registerClass' :: RegisterClassFunc GodotWestonCompositor)


registerClass' :: forall a . ClassExport a
  => GdnativeHandle -> (GodotObject -> IO a) -> IO ()
registerClass' desc constr = do
  registerClass desc extends constr destr
  forM (classMethods @a) (registerMethod' desc)
    >>= printRegistered (godotClassName @a) extends
 where
  extends   = T.unpack $ classExtends @a
  destr _ _ = return ()


registerMethod'
  :: forall a . GodotClass a
  => GdnativeHandle
  -> Func a
  -> IO String
registerMethod' desc (Func rpc name func) = do
  let name' = T.unpack name
  registerMethod desc (name') (rpcMode rpc) func
  return name'


printRegistered :: String -> String -> [String] -> IO ()
printRegistered clsnm extends methods =
  godotPrint $ T.pack $ unlines
    [ "Registered class:"
    , "  Name: " ++ clsnm
    , "  Extends: " ++ extends
    , "  Member methods:"
    ] ++ unlines (map ("    " ++) methods)


rpcMode :: RPC -> GodotMethodRpcMode
rpcMode rpc = case rpc of
  NoRPC  -> GodotMethodRpcModeDisabled
  Remote -> GodotMethodRpcModeRemote
  Sync   -> GodotMethodRpcModeSync
  Master -> GodotMethodRpcModeMaster
  Slave  -> GodotMethodRpcModeSlave
