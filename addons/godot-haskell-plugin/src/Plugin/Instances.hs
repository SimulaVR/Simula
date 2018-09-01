{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Instances where

import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types


-- We need these instances for functions that requires them

type instance TypeOf 'HaskellTy Bool = Bool
instance GodotFFI Bool Bool where
  fromLowLevel = return
  toLowLevel = return

type instance TypeOf 'HaskellTy Float = Float
instance GodotFFI Float Float where
  fromLowLevel = return
  toLowLevel = return

type instance TypeOf 'HaskellTy Int = Int
instance GodotFFI Int Int where
  fromLowLevel = return
  toLowLevel = return

type instance TypeOf 'HaskellTy () = ()
instance GodotFFI () () where
  fromLowLevel = return . const ()
  toLowLevel = return . const ()

type instance TypeOf 'HaskellTy GodotObject = GodotObject
instance GodotFFI GodotObject GodotObject where
  fromLowLevel = return
  toLowLevel = return
