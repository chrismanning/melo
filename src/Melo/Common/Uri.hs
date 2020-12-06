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

uriToFilePath :: URI -> Maybe FilePath
uriToFilePath uri =
  case uriScheme uri of
    "file:" -> Just $ unEscapeString (uriPath uri)
    _ -> Nothing
