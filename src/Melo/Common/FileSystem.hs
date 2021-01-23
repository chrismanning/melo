{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.FileSystem where

import Control.Algebra
import Control.Carrier.Lift
import Control.Effect.TH
import Control.Exception.Safe
import Data.ByteString as BS
import GHC.IO.Exception
import Melo.Common.Effect
import Melo.Common.Logging
import qualified System.Directory as Dir
import System.FilePath
import System.IO
import System.IO.Error hiding (catchIOError)

data FileSystem :: Effect where
  DoesFileExist :: FilePath -> FileSystem m Bool
  DoesDirectoryExist :: FilePath -> FileSystem m Bool
  ListDirectory :: FilePath -> FileSystem m [FilePath]
  CanonicalizePath :: FilePath -> FileSystem m FilePath
  ReadFile :: FilePath -> FileSystem m ByteString
  MovePath :: FilePath -> FilePath -> FileSystem m (Either MoveError ())

data MoveError
  = WouldOverwrite
  | SourceDoesNotExist
  | SomeIOError IOError
  deriving (Show)

instance Exception MoveError

makeSmartConstructors ''FileSystem

newtype FileSystemIOC m a = FileSystemIOC
  { runFileSystemIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

runFileSystemIO :: FileSystemIOC m a -> m a
runFileSystemIO = runFileSystemIOC

instance
  ( Has (Lift IO) sig m,
    Has Logging sig m
  ) =>
  Algebra (FileSystem :+: sig) (FileSystemIOC m)
  where
  alg _ (L sig) ctx =
    ctx $$!> sendIO case sig of
      DoesFileExist p -> Dir.doesFileExist p
      DoesDirectoryExist p -> Dir.doesDirectoryExist p
      ListDirectory p -> Dir.listDirectory p
      CanonicalizePath p -> Dir.canonicalizePath p
      ReadFile p -> withBinaryFile p ReadMode BS.hGetContents
      MovePath a b -> movePathIO a b
  alg hdl (R other) ctx = FileSystemIOC (alg (runFileSystemIOC . hdl) other ctx)

movePathIO :: FilePath -> FilePath -> IO (Either MoveError ())
movePathIO a b =
  Dir.doesPathExist b >>= \case
    True -> pure $ Left WouldOverwrite
    False -> do
      Dir.doesFileExist a >>= \case
        True -> do
          Dir.createDirectoryIfMissing True (takeDirectory b)
          catchIOError (Right <$> Dir.renameFile a b) $ \e -> do
            case ioeGetErrorType e of
              UnsupportedOperation -> do
                Dir.copyFileWithMetadata a b
                pure $ Right ()
              _ -> do
                if isDoesNotExistError e
                  then do
                    movePathIO a b
                  else pure $ Left $ SomeIOError e
        False -> do
          Dir.doesDirectoryExist a >>= \case
            True -> error "directory move not implemented"
            False -> pure $ Left SourceDoesNotExist
