module Plugin.Util.Operators where

import           Godot.Gdnative.Types


-- | Convenience function for uninterrupted applicative chains
(<^>) :: (Monad m) => m (a -> m b) -> a -> m b
f <^> x = f <*> pure x >>= id

infixl 4 <^>


-- Low/High level conversions


(<$>#) :: GodotFFI low high => (high -> a) -> low -> IO a
f <$># a = f <$> (fromLowLevel a)

infixl 4 <$>#


(#<$>) :: GodotFFI low high => (low -> a) -> high -> IO a
f #<$> a = f <$> (toLowLevel a)

infixl 4 #<$>


(>>=#) :: GodotFFI low high => IO high -> (low -> IO a) -> IO a
a >>=# f = a >>= toLowLevel >>= f

(#=<<) :: GodotFFI low high => (low -> IO a) -> IO high -> IO a
f #=<< a = f =<< toLowLevel =<< a

infixl 1 >>=#
infixr  1 #=<<


(#>>=) :: GodotFFI low high => IO low -> (high -> IO a) -> IO a
a #>>= f = a >>= fromLowLevel >>= f

(=<<#) :: GodotFFI low high => (high -> IO a) -> IO low -> IO a
(=<<#) = flip (#>>=)

infixl 1 #>>=
infixr 1 =<<#


(#>>) :: GodotFFI low high => low -> (high -> IO a) -> IO a
a #>> f = fromLowLevel a >>= f

(<<#) :: GodotFFI low high => (high -> IO a) -> low -> IO a
(<<#) = flip (#>>)

infixl 1 #>>
infixr 1 <<#


(>>#) :: GodotFFI low high => high -> (low -> IO a) -> IO a
a >># f = toLowLevel a >>= f

(#<<) :: GodotFFI low high => (low -> IO a) -> high -> IO a
f #<< a = f =<< toLowLevel a
infixr 1 #<<
infixl 1 >>#


(>>|=)
  :: (GodotFFI low high, GodotFFI low' high')
  => IO high -> (low -> IO low') -> IO high'
high >>|= lowf = high >>= toLowLevel >>= lowf >>= fromLowLevel

(=|<<)
  :: (GodotFFI low high, GodotFFI low' high')
  => (low -> IO low') -> IO high -> IO high'
(=|<<) = flip (>>|=)

infixl 1 >>|=
infixr 1 =|<<


(>|=)
  :: (GodotFFI low high, GodotFFI low' high')
  => high -> (low -> IO low') -> IO high'
(>|=) = (>>|=) . return

(=|<)
  :: (GodotFFI low high, GodotFFI low' high')
  => (low -> IO low') -> high -> IO high'
(=|<) = flip (>|=)

infixr 1 >|=
infixr 1 =|<
