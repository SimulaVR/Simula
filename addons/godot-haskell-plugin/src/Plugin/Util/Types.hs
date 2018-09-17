{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
module Plugin.Util.Types where

import           Godot.Gdnative.Types
import           Godot.Gdnative.Internal
import           Godot.Internal.Dispatch
import qualified Godot.Methods as G
import           Data.Text               as T
import           Data.Function           ((&))

-- | Type to help with type conversions
data HighLevelOf low where
  High
    :: forall low high. (GodotFFI low high, high ~ (TypeOf 'HaskellTy low))
    => high
    -> HighLevelOf low


withHigh
  :: forall low high high'
   . (GodotFFI low high, high ~ (TypeOf 'HaskellTy low))
  => (high -> high')
  -> HighLevelOf low
  -> high'
withHigh f (High high) = f high


asClass ::  (GodotObject :< a, a :< b) => (GodotObject -> b) -> Text -> a -> IO (Maybe b)
asClass constr cls a = do
  isClass <- is_class a cls
  return $ if isClass
    then Just $ constr (safeCast a)
    else Nothing

asClass' :: (GodotObject :< a, a :< b) => (GodotObject -> b) -> Text -> a -> IO b
asClass' constr cls a =
  asClass constr cls a >>= \case
    Just a' -> return a'
    Nothing -> error $ unpack $ "Could not cast to " `append` cls

asObj :: (GodotObject :< a) => a -> GodotObject
asObj a = safeCast a

is_class :: GodotObject :< a => a -> Text -> IO Bool
is_class obj cls = do
  clsStr <- toLowLevel cls
  G.is_class (asObj obj) clsStr


{-
 -asClass
 -  :: forall a b
 -   . (GodotClass a, GodotClass b, GodotObject :< a, a :< b)
 -  => (GodotObject -> b)
 -  -> a
 -  -> IO (Maybe b)
 -asClass constr a = do
 -  let obj = safeCast a :: GodotObject
 -  isSubclass <- G.is_class obj #<< (pack $ godotClassName @b)
 -  return $ if isSubclass then Just $ constr obj else Nothing
 -}
