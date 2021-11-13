{-# LANGUAGE QuasiQuotes #-}

module Melo.Lookup.MusicBrainz.Mock where

import Control.Monad.Mock.TH
import Melo.Lookup.MusicBrainz

makeAction "MusicBrainzServiceAction" [ts| MusicBrainzService |]
