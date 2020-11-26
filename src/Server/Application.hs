{-# LANGUAGE OverloadedStrings #-}

module Server.Application (
    app
) where

import Network.Wai ( responseLBS, Application )
import Network.HTTP.Types ( status200 )
import Network.HTTP.Types.Header ( hContentType )

import Server.Router ( )

app :: Application
app request respond =
    respond $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"
