module Melo.Common.Http
  (
    meloUserAgent,
  )
where

import Data.String (IsString)

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"
