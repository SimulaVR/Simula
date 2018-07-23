{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Util where

import           Data.Function           ((&))
import           Data.Text               as T

import           Foreign.C               (withCString)

import           Godot.Api               (Godot_ClassDB (..))
import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types
import           Godot.Methods           (get_class, instance')

import           Plugin.Util.Operators



godotPrint :: Text -> IO ()
godotPrint str = godot_print #<< str


mkClassInstance :: Text -> IO GodotObject
mkClassInstance className = do
  classDB <- Godot_ClassDB <$> getSingleton "ClassDB"
  cls <- className
    >># instance' classDB
    >>= fromGodotVariant
  get_class cls
    #>>= godotPrint . mappend "I made a "
  return cls


getSingleton :: Text -> IO GodotObject
getSingleton name = godot_global_get_singleton
  & withCString (T.unpack name)


-- Extras


type instance TypeOf 'HaskellTy () = ()
instance GodotFFI () () where
  fromLowLevel = return . const ()
  toLowLevel = return . const ()
