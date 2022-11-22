{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Internal.Locate where

import Control.Foldl qualified as F
import Data.Binary.Get
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Function
import Numeric.Natural
import Streaming.ByteString qualified as S
import Streaming.Binary qualified as S
import Streaming.Prelude qualified as S (slidingWindow, foldM_)

findSubstring :: Monad m => BS.ByteString -> S.ByteStream m r -> m (Maybe Natural)
findSubstring n _ | n == BS.empty = pure $ Just 0
findSubstring n h = let !n' = BS.unpack n in
  S.slidingWindow (BS.length n) (S.unpack h)
    & F.impurely S.foldM_ (F.generalize $ F.findIndex (\s -> toList s == n'))
    & fmap (fmap fromIntegral)

findBinary :: forall a m r. Monad m => Get a -> S.ByteStream m r -> m (S.ByteStream m r, Maybe a)
findBinary getter stream = loop 0 stream
  where
    loop :: Natural -> S.ByteStream m r -> m (S.ByteStream m r, Maybe a)
    loop i stream = do
      (rest, _n, r) <- S.decodeWith (lookAhead getter) stream
      case r of
        Right a -> pure (rest, Just a)
        Left e -> do
          S.uncons rest >>= \case
            Left r -> pure (pure r, Nothing)
            Right (_b, rest) -> do
              loop (i + 1) rest
