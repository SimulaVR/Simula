{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Types where

import           Data.Text               (Text)
import           Data.Vector             (Vector)

import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types
import           Godot.Nativescript      (GdnativeHandle, GodotClass (..))

-- Just lazily trying things and don't want to modify
-- godot-haskell's GodotClass
class GodotClass a => ClassExport a where
  classInit :: GodotObject -> IO a
  classExtends :: Text
  classMethods :: [Func a]

type GodotFunc a = GodotObject -> a -> Vector GodotVariant -> IO GodotVariant

data Func a = Func { funcRPC :: RPC, funcName :: Text, funcGodot :: GodotFunc a }

data RPC
  = NoRPC
  | Remote
  | Sync
  | Master
  | Slave


-- Until implemented in godot-haskell

data ObjectRef = ObjectRef GodotObject
type instance TypeOf 'HaskellTy GodotObject = ObjectRef
instance GodotFFI GodotObject ObjectRef where
  fromLowLevel o = return $ ObjectRef o
  toLowLevel (ObjectRef o) = return o
