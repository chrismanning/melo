{-# LANGUAGE QuasiQuotes #-}

module Melo.Metadata.Aggregate.Mock where

import Control.Monad.Mock.TH
import Melo.Metadata.Aggregate

makeAction "MetadataAggregateAction" [ts| MetadataAggregate |]
