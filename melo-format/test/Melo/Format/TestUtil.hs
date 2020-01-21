module Melo.Format.TestUtil where

import Control.Exception.Safe
import Data.Conduit
import Data.Conduit.Combinators
import System.IO
import System.IO.Temp

withTempCopyOf :: FilePath -> (Handle -> IO ()) -> IO ()
withTempCopyOf op f = do
  tmpDir <- getCanonicalTemporaryDirectory
  bracket
    (openBinaryTempFile tmpDir "melo-id3v1-test-")
    (\(_, h) -> hClose h)
    ( \(_, h) -> do
        runConduitRes $ sourceFile op .| sinkHandle h
        f h
    )
