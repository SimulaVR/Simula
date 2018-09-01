{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Plugin.Util.Variants where

import           Control.Monad
import           Data.Function           ((&))
import           Data.Typeable
import           Data.Vector

import           Godot.Gdnative.Types
import           Godot.Gdnative.Internal

import           Plugin.Util.Types


-- | Container for a variant type with the type variable
--   holding the expected type of the variant
data VariantOf low where
  VT
    :: forall low high. (AsVariant low, GodotFFI low high)
    => GodotVariant
    -> VariantOf low


-- | Convert a variant type into a Haskell type
highFromVariant
  :: forall low high
   . ( Typeable low
     , AsVariant low
     , GodotFFI low high
     , high ~ (TypeOf 'HaskellTy low)
     )
  => Bool
  -> VariantOf low
  -> IO (HighLevelOf low)
highFromVariant shouldDestroy (VT v) = do
  mayLow <- (fromVariant <$> fromLowLevel v :: IO (Maybe low))
  case mayLow of
    Just low -> do
      res <- High <$> fromLowLevel low
      when shouldDestroy $ do
        res `seq` godot_variant_destroy v
      return res
    Nothing -> error "Failed to convert variant"


-- | Convenience function for declaring the final return value.
-- Spares you from having to juggle conversions between high/low level and
-- variants.
-- Return a Text value (as GodotString):
-- @retn (High "Hello there" :: HighLevelOf GodotString)@.
retn
  :: forall low high
   . (AsVariant low, GodotFFI low high, high ~ TypeOf 'HaskellTy low)
  => HighLevelOf low
  -> IO GodotVariant
retn (High high) = do
  low <- high & (toLowLevel :: high -> IO low)
  let vt = low & (toVariant :: low -> Variant 'GodotTy)
  lowVt <- vt & toLowLevel
  return lowVt

-- | A more direct way of returning null.
retnil :: IO GodotVariant
retnil = godot_variant_new_nil


-- | Convert the argument variant type into a proper type
withVariant :: (Typeable a, AsVariant a) => (a -> IO b) -> GodotVariant -> IO b
withVariant f v = fromGodotVariant v >>= f


-- | Get argument 'n' in the variant arguments 'Vector'
getArg
  :: forall a. (Typeable a , AsVariant a)
  => Int
  -> Vector GodotVariant
  -> IO (Maybe a)
getArg n args = args !? n & \case
  Just vt -> Just <$> fromGodotVariant vt
  Nothing -> return Nothing


-- | Unsafe version of getArg. Only use when you're sure the argument exists!
-- An example of appropriate use is getting the delta value from '_process'.
getArg'
  :: forall a. (Typeable a , AsVariant a)
  => Int
  -> Vector GodotVariant
  -> IO a
getArg' n args = args !? n & \case
  Just vt -> fromGodotVariant vt
  Nothing -> error "Failed to get argument."


-- | Get argument 'n' in the variant arguments 'Vector' as a Haskell type
getHighArg
  :: forall low high
   . ( Typeable low
     , AsVariant low
     , GodotFFI low high
     , high ~ (TypeOf 'HaskellTy low)
     )
  => Int
  -> Vector GodotVariant
  -> IO (Maybe (HighLevelOf low))
getHighArg n args = args !? n & \case
  Just vt -> Just <$> highFromVariant False (VT vt)
  Nothing -> return Nothing


-- | Unsafe version of getHighArg
getHighArg'
  :: forall low high
   . ( Typeable low
     , AsVariant low
     , GodotFFI low high
     , high ~ (TypeOf 'HaskellTy low)
     )
  => Int
  -> Vector GodotVariant
  -> IO (HighLevelOf low)
getHighArg' n args =
  getHighArg n args >>= \case
    Just arg -> return arg
    Nothing -> error "Failed to get argument."
