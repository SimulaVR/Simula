{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Util where

import           Data.Function           ((&))
import           Data.Text               as T

import           Foreign.C               (withCString)

import           Godot.Api               
import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types
import           Godot.Methods           (get_class, instance')
import qualified Godot.Methods as G
import Godot.Internal.Dispatch 

import           Plugin.Util.Operators



godotPrint :: Text -> IO ()
godotPrint str = godot_print #<< str


mkClassInstance :: Text -> IO GodotObject
mkClassInstance className = do
  classDB <- getClassDB
  cls <- className
    >># instance' classDB
    >>= fromGodotVariant
  return cls


getSingleton :: Text -> IO GodotObject
getSingleton name = godot_global_get_singleton
  & withCString (T.unpack name)


getClassDB :: IO Godot_ClassDB
getClassDB = Godot_ClassDB <$> getSingleton "ClassDB"

getVisualServer :: IO GodotVisualServer
getVisualServer = GodotVisualServer <$> getSingleton "VisualServer"

getResourceLoader :: IO Godot_ResourceLoader
getResourceLoader = Godot_ResourceLoader <$> getSingleton "ResourceLoader"
  

is_class :: GodotObject :< a => a -> Text -> IO Bool
is_class obj cls = do
  clsStr <- toLowLevel cls
  G.is_class (safeCast obj :: GodotObject) clsStr

-- Extras


type instance TypeOf 'HaskellTy () = ()
instance GodotFFI () () where
  fromLowLevel = return . const ()
  toLowLevel = return . const ()
