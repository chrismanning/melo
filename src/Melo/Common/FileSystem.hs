{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.FileSystem where

import Control.Algebra
import Control.Carrier.Lift
import Control.Effect.TH
import Melo.Common.Effect
import Melo.Common.Logging
import qualified System.Directory as Dir

data FileSystem :: Effect where
  DoesFileExist :: FilePath -> FileSystem m Bool
  DoesDirectoryExist :: FilePath -> FileSystem m Bool
  ListDirectory :: FilePath -> FileSystem m [FilePath]
  CanonicalizePath :: FilePath -> FileSystem m FilePath

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
  alg hdl (R other) ctx = FileSystemIOC (alg (runFileSystemIOC . hdl) other ctx)
