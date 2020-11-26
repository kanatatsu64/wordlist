{-# LANGUAGE OverloadedStrings #-}

module Server.Response (
    sample,
    notFound
) where

import Network.HTTP.Types.Status ( status200, status404 )
import Network.Wai ( responseLBS )
import Network.HTTP.Types.Header ( hContentType )

sample = responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

notFound = responseLBS status404 [] ""
