{-# LANGUAGE QuasiQuotes #-}

module Melo.Common.Metadata.Mock where

import Control.Monad.Mock.TH
import Melo.Common.Metadata

makeAction "MetadataAggregateAction" [ts| MetadataAggregate |]
