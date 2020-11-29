module Melo.Common.Uri where

import Network.URI

fileUri :: FilePath -> URI
fileUri p =
  URI
    { uriScheme = "file:",
      uriAuthority = Nothing,
      uriPath = escapeURIString (\c -> isUnreserved c || c == '/') p,
      uriQuery = "",
      uriFragment = ""
    }
