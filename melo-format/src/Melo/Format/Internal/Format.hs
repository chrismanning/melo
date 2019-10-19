{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Internal.Format where

class MetadataFormat a where

  formatDesc :: String

  formatDesc' :: a -> String
  formatDesc' _ = formatDesc @a
