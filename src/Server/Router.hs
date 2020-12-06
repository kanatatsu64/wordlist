{-# LANGUAGE OverloadedStrings #-}

module Server.Router (
    root
) where

import Server.Internal.Router ( responder, get, mount, (~>) )
import Server.Handler ( sample, static )
import qualified Server.Api.Router as Api ( router )

router = do
    get "/" ~> get "/index.html"
    mount "/api" Api.router
    get "/sample" sample

    get "/redirect" ~> get "/"

    {- static files -}
    get "/resource/*" $ static "resource/"
    get "/*" $ static "client/dist/"

root = responder router
