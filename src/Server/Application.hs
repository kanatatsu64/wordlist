{-# LANGUAGE OverloadedStrings #-}

module Server.Application (
    app
) where

import Network.Wai ( Application )

import Server.Router ( responder )

app :: Application
app request respond = respond =<< responder request
