{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.CubeMaker where

import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T
import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import           Godot.Gdnative.Types        (GodotFFI, LibType, TypeOf)
import qualified Godot.Methods               as G



--- TYPES

data CubeMaker = CubeMaker
  { _cmObj      :: GodotObject
  , _cmChildren :: TVar [GodotNode]
  , _cmTest     :: TVar Text
  }

instance GodotClass CubeMaker where
  godotClassName = "CubeMaker"

instance ClassExport CubeMaker where
  classInit obj = atomically $
    CubeMaker obj <$> newTVar [] <*> newTVar ""
  classExtends = "Node"
  classMethods = [ Func NoRPC "_ready" ready
                 , Func NoRPC "_physics_process" physicsProcess
                 , Func NoRPC "make_cube" makeCube
                 ]

instance HasBaseClass CubeMaker where
  type BaseClass CubeMaker = GodotNode
  super (CubeMaker obj _ _) = GodotNode obj



--- FUNCTIONS


ready :: GodotFunc CubeMaker
ready _ self _ = do
  godotPrint "_ready from Haskell"
  G.call self <$> (toLowLevel "make_cube") <^> []
  toLowLevel VariantNil


physicsProcess :: GodotFunc CubeMaker
physicsProcess _ self@(CubeMaker obj cs txt) args = do
  delta <- args !? 0
    & maybe (return 0) fromGodotVariant :: IO Float

  let getname (GodotNode obj) =
        obj ^. (Get "name" :: Property GodotString) :: IO Text

  names <- atomically (readTVar cs)
    >>= mapM getname
    >>= (T.unlines >>> return)

  godotPrint names

  children <- G.get_children self
    #>>= (mapMaybe fromVariant
      >>> toList
      >>> return)
    :: IO [GodotNode]

  atomically $ writeTVar cs children


  (GodotNode mshi) <- G.get_child self 0
  (TF bs pos) <- fromLowLevel =<< G.get_global_transform (GodotSpatial mshi)

  let newpos = pos ^+^ (V3 1 0 0) ^* delta

  G.set_global_transform (GodotMeshInstance obj) #<< TF bs newpos
  godotPrint . pack $ show newpos

  retn (HighLevel () :: HighLevelOf ())



makeCube :: GodotFunc CubeMaker
makeCube _ self _ = do
  msh  <- mkClassInstance "CubeMesh"
  mshi <- mkClassInstance "MeshInstance"
  mkClassInstance "CubeMesh"
    >>= (GodotMeshInstance mshi & G.set_mesh) . GodotMesh
  G.add_child self mshi True
  toLowLevel . VariantString #<< "Wee"


-- This should ideally be `Vector (Variant 'HaskellTy)`, but that would
-- require `AsVariant` to handle both `LibType`s.
type instance TypeOf 'HaskellTy GodotArray = Vector (Variant 'GodotTy)
instance GodotFFI GodotArray (Vector (Variant 'GodotTy)) where
  fromLowLevel vs = do
    size <- fromIntegral <$> Api.godot_array_size vs
    unfoldrNM size variantAt 0
    where
      variantAt n =
        let wrapInc v = Just (v, n + 1)
        in wrapInc <$> (Api.godot_array_get vs n >>= fromLowLevel)
  toLowLevel vs = do
    array <- Api.godot_array_new
    mapM_ (toLowLevel >=> Api.godot_array_append array) vs
    return array
