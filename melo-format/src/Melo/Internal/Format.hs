{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Format where

class MetadataFormat a where
  formatDesc :: String
  formatDesc' :: a -> String
  formatDesc' _ = formatDesc @ a
