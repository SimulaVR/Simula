{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Plugin.Util.Variants where

import           Control.Category        ((>>>))
import           Control.Monad           ((>=>))
import           Data.Function           ((&))
import           Data.Text               as T

import           Godot.Gdnative.Types
import           Godot.Gdnative.Internal
import           Godot.Methods           (get, get_class, instance', set)

import Plugin.Util.Operators

-- | Container for a variant type with the type variable
--   holding the expected type of the variant
data VariantOf a = VT GodotVariant


-- | Data type to help with required type annotation
data Property a = Get Text


-- | Another data type to help with required type annotation
data HighLevelOf low = HighLevel (TypeOf 'HaskellTy low)


-- Properties

-- | Get the value of a property
(^.)
  :: forall low high. (AsVariant low, GodotFFI low high)
  => GodotObject -- ^ Base object
  -> Property low -- ^ Contains property name
  -> IO high -- ^ Resulting Haskell type
base ^. p =
  base ^? p >>= \case
    Just a -> a `seq` return a
    Nothing -> error "Error in API: Couldn't fromVariant"


-- | Safe version of (^.)
(^?) :: forall low high. (AsVariant low, GodotFFI low high)
  => GodotObject -> Property low -> IO (Maybe high)
base ^? (Get prop) =
  (get base #<< prop)
    >>= (hsFromVariant . (VT :: GodotVariant -> VariantOf low))

infixl 8 ^., ^?


-- | Set the value of a property (for intuitive usage, use '&',
-- like so: @obj & (Get "name" :: Property GodotString) "NewName"@)
(.~) :: forall low high. (AsVariant low, GodotFFI low high)
  => Property low -> high -> GodotObject -> IO GodotObject
(Get prop) .~ val = \base -> do
  vt <- toLowLevel val
    >>= (toVariant >>> toLowLevel :: low -> IO GodotVariant)

  set base <$> toLowLevel prop <^> vt
    >> return base

infixr 4 .~


(<~>) :: AsVariant a => (Variant 'GodotTy -> b) -> a -> b
f <~> a = f (toVariant a)

infixl 4 <~>


(<~~>) :: (Monad m, AsVariant a) => m ([Variant 'GodotTy] -> m b) -> [a] -> m b
f <~~> xs = f <^> (toVariant <$> xs)

infixl 4 <~~>


retn
  :: forall low high. (AsVariant low, GodotFFI low high, high ~ TypeOf 'HaskellTy low)
  => HighLevelOf low
  -> IO GodotVariant
retn (HighLevel high) = high
  & (toLowLevel :: high -> IO low)
  >>= ((toVariant :: low -> Variant 'GodotTy)
        >>> (toLowLevel :: Variant 'GodotTy -> IO GodotVariant))

retnil = godot_variant_new_nil


hsFromVariant
  :: forall low high. (AsVariant low, GodotFFI low high)
  => VariantOf low -> IO (Maybe high)
hsFromVariant (VT v) =
  (fromVariant <$> fromLowLevel v :: IO (Maybe low))
    >>= maybe (return Nothing) justLow
  where
    justLow = (fromLowLevel :: low -> IO high)
      >=> return . Just


withVariant :: AsVariant a => (a -> IO b) -> GodotVariant -> IO b
withVariant f v = fromGodotVariant v >>= f
