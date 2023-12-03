{-# LANGUAGE QuasiQuotes #-}
module Melo.Lookup.Covers.Mock where

import Control.Monad.Mock.TH
import Melo.Lookup.Covers

makeAction "CoverServiceAction" [ts| CoverService |]
