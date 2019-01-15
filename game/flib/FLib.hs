{-# LANGUAGE ForeignFunctionInterface #-}
module FLib where

import           Data.Coerce             (coerce)

import           Foreign                 (peek)

import           Godot.Gdnative.Internal
import           Godot.Nativescript

import           Plugin



godot_nativescript_init :: GdnativeHandle -> IO ()
godot_nativescript_init desc = do
  putStrLn "Haskell NativeScript initialized"
  registerClasses desc

foreign export ccall godot_nativescript_init :: GdnativeHandle -> IO ()



godot_gdnative_init :: GodotGdnativeInitOptionsPtr -> IO ()
godot_gdnative_init optPtr = do
  putStrLn "Haskell GDNative initialized"
  opt <- peek optPtr
  initApiStructs opt

foreign export ccall godot_gdnative_init :: GodotGdnativeInitOptionsPtr -> IO ()



godot_gdnative_terminate :: GodotGdnativeTerminateOptionsPtr -> IO ()
godot_gdnative_terminate optPtr = putStrLn "Haskell GDNative terminated"

foreign export ccall godot_gdnative_terminate :: GodotGdnativeTerminateOptionsPtr -> IO ()
