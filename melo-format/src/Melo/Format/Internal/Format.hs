{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Internal.Format where

import Data.Text (Text)

class MetadataFormat a where

  formatDesc :: Text

  formatDesc' :: a -> Text
  formatDesc' _ = formatDesc @a
