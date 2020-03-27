module Melo.Common.Effect where

import Data.Kind (Type)

type Effect = (Type -> Type) -> Type -> Type
