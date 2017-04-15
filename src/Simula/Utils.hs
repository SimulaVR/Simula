module Simula.Utils where

import Control.Monad

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import Foreign hiding (void)
import Foreign.C.String

import System.Environment

withCArgs :: (VM.IOVector CString -> IO ()) -> IO ()
withCArgs act = do
  cargs <- getArgs >>= mapM newCString
  vec <- V.thaw $ V.fromList cargs
  act vec
  mapM_ free cargs
  
