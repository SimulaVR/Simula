module Plugin.Imports
  ( module B
  , module D
  , module G
  , module P
  ) where

-- Things I wish were in Prelude
import           Control.Category                 as B ((<<<), (>>>))
import           Control.Monad                    as B ((<=<), (>=>))
import           Control.Lens                     as B ((^.), (.~), (%~))
import           Data.Function                    as B
import           Data.Functor                     as B ((<&>))
import           Data.Text                        as B (Text, append, pack,
                                                        unpack)

-- Dependencies
import           Control.Concurrent.STM.TVar      as D
import           Control.Monad.STM                as D
import           Data.Vector                      as D (Vector, mapMaybe,
                                                        toList, unfoldrNM, (!?))
import           Linear                           as D

-- Godot
import           Godot.Api                        as G
import           Godot.Gdnative.Internal.Gdnative as G hiding (withGodotString)

-- Hiding things you shouldn't have to bother with
import           Godot.Gdnative.Types             as G hiding (AsVariant,
                                                        GodotFFI, LibType,
                                                        TypeOf)
import           Godot.Internal.Dispatch          as G

-- import           Godot
import           Godot.Nativescript

import System.Clock as P
import Control.Monad.Extra as P (when, whenM)
