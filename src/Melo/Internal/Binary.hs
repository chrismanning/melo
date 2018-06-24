{-# LANGUAGE UndecidableInstances #-}

module Melo.Internal.Binary
  ( BinaryGet(..)
  , bdecode
  , BinaryPut(..)
  ) where

import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as L

class BinaryGet a where
  bget :: Bin.Get a

bdecode :: BinaryGet a => L.ByteString -> a
bdecode = Bin.runGet bget

instance {-# OVERLAPPABLE #-} Bin.Binary a => BinaryGet a where
  bget = Bin.get

class BinaryPut a where
  put :: a -> Bin.Put

instance {-# OVERLAPPABLE #-} Bin.Binary a => BinaryPut a where
  put = Bin.put
