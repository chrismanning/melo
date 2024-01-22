{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.FileSystem where

import Control.Monad.Extra
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.IO.Exception
import Melo.Common.Exception
import Melo.Common.Logging
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as P
import System.IO hiding (readFile)
import System.IO.Error hiding (catchIOError)
import Prelude hiding (readFile)

class (Monad m) => FileSystem m where
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  listDirectory :: FilePath -> m [FilePath]
  canonicalizePath :: FilePath -> m FilePath
  readFile :: FilePath -> m ByteString
  movePath :: FilePath -> FilePath -> m (Either FileManipError ())
  copyPath :: FilePath -> FilePath -> m (Either FileManipError ())
  removePath :: FilePath -> m (Either RemoveError ())
  removeEmptyDirectories :: FilePath -> m ()

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
  copyPath a b = lift (copyPath a b)
  removePath = lift . removePath
  removeEmptyDirectories = lift . removeEmptyDirectories

data FileManipError
  = WouldOverwrite
  | SourceDoesNotExist
  | FileManipIOError IOError
  deriving (Show)
  deriving (TextShow) via FromStringShow FileManipError

instance Exception FileManipError

data RemoveError
  = DirectoryNotEmpty
  | DirectoryDoesNotExist
  | RemoveIOError IOError
  deriving (Show)
  deriving (TextShow) via FromStringShow RemoveError

instance Exception RemoveError

instance FileSystem IO where
  doesFileExist p = liftIO $ Dir.doesFileExist p
  doesDirectoryExist p = liftIO $ Dir.doesDirectoryExist p
  listDirectory p = liftIO $ fmap (p </>) <$> Dir.listDirectory p
  canonicalizePath p = liftIO $ Dir.canonicalizePath p
  readFile p = liftIO $ withBinaryFile p ReadMode BS.hGetContents
  movePath a b = liftIO $ movePathIO a b
  copyPath a b =
    liftIO $
      catchIOError (Dir.createDirectoryIfMissing True (P.takeDirectory b) >> Dir.copyFileWithMetadata a b >> pure (Right ())) (pure . Left . FileManipIOError)
  removePath p = liftIO $ handleIOError (pure . Left . RemoveIOError) do
    whenM (Dir.doesDirectoryExist p) (Dir.removeDirectory p)
    whenM (Dir.doesFileExist p) (Dir.removeFile p)
    pure $ Right ()
  removeEmptyDirectories dir =
    doesDirectoryExist dir >>= \case
      False -> pure ()
      True ->
        handleIO
          (const $ pure ())
          ( listDirectory dir >>= \case
              [] -> do
                liftIO $ Dir.removeDirectory dir
                $(logInfo) $ "Removed directory " <> showt dir
                pure ()
              es -> forM_ es removeEmptyDirectories
          )

movePathIO :: FilePath -> FilePath -> IO (Either FileManipError ())
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
                  else pure $ Left $ FileManipIOError e
        False -> do
          Dir.doesDirectoryExist a >>= \case
            True -> error "directory move not implemented"
            False -> pure $ Left SourceDoesNotExist
