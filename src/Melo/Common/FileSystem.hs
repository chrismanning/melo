{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.FileSystem where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.IO.Exception
import qualified System.Directory as Dir
import qualified System.FilePath as P
import System.IO hiding (readFile)
import System.IO.Error hiding (catchIOError)
import Prelude hiding (readFile)

class Monad m => FileSystem m where
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  listDirectory :: FilePath -> m [FilePath]
  canonicalizePath :: FilePath -> m FilePath
  readFile :: FilePath -> m ByteString
  movePath :: FilePath -> FilePath -> m (Either MoveError ())

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    FileSystem m
  ) =>
  FileSystem (t m)
  where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile = lift . readFile
  movePath a b = lift (movePath a b)

data MoveError
  = WouldOverwrite
  | SourceDoesNotExist
  | SomeIOError IOError
  deriving (Show)

instance Exception MoveError

newtype FileSystemT m a = FileSystemT
  { runFileSystemT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask)
  deriving (MonadTrans, MonadTransControl) via IdentityT

runFileSystemIO :: FileSystemT m a -> m a
runFileSystemIO = runFileSystemT

instance
  ( MonadIO m
  ) =>
  FileSystem (FileSystemT m)
  where
  doesFileExist p = liftIO $ Dir.doesFileExist p
  doesDirectoryExist p = liftIO $ Dir.doesDirectoryExist p
  listDirectory p = liftIO $ Dir.listDirectory p
  canonicalizePath p = liftIO $ Dir.canonicalizePath p
  readFile p = liftIO $ withBinaryFile p ReadMode BS.hGetContents
  movePath a b = liftIO $ movePathIO a b

movePathIO :: FilePath -> FilePath -> IO (Either MoveError ())
movePathIO a b =
  Dir.doesPathExist b >>= \case
    True -> pure $ Left WouldOverwrite
    False -> do
      Dir.doesFileExist a >>= \case
        True -> do
          Dir.createDirectoryIfMissing True (P.takeDirectory b)
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
