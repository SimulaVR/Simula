module Simula.Utils where

import Control.Monad

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import Foreign hiding (void)
import Foreign.C.String

import System.Environment

--todo: bracket pattern
withCStringVector :: [String] -> (VM.IOVector CString -> IO a) -> IO a
withCStringVector args act = do
  cargs <- mapM newCString args
  vec <- V.thaw $ V.fromList cargs
  res <- act vec
  mapM_ free cargs
  return res
  
withCArgs :: (VM.IOVector CString -> IO ()) -> IO ()
withCArgs act = getArgs >>= \args -> withCStringVector args act
