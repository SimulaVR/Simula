{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Types where

import           Data.Text               (Text)
import           Data.Vector             (Vector)

import           Godot.Gdnative.Internal
import           Godot.Nativescript      (GodotClass (..))

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
