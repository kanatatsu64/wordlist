{-# LANGUAGE OverloadedStrings #-}

module Server.Response (
    sample,
    html,
    notFound
) where

import Network.HTTP.Types.Status ( status200, status404 )
import Network.Wai ( responseLBS, Response )
import Network.HTTP.Types.Header ( hContentType )

import Server.Types ( lazyEncode )

sample = responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

html :: String -> Response
html str = responseLBS status200 [(hContentType, "text/html")] $ lazyEncode str

notFound = responseLBS status404 [] ""
