{-# LANGUAGE TypeInType #-}

module Melo.Format.Internal.TypeLevel where

type family FMap (m :: a -> b) (x :: f a) :: f b

type instance FMap m '[] = '[]

type instance FMap m (v ': vs) = m v ': FMap m vs
