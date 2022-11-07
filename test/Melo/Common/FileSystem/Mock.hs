{-# LANGUAGE QuasiQuotes #-}
module Melo.Common.FileSystem.Mock where

import Control.Monad.Mock.TH
import Melo.Common.FileSystem

makeAction "FileSystemAction" [ts| FileSystem |]
