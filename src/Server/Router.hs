{-# LANGUAGE OverloadedStrings #-}

module Server.Router (
    root
) where

import Server.Internal.Router ( Router, responder, get, mount, (~>) )
import Server.Handler ( sample, static )
import qualified Server.Csv.Router as Csv ( router )
import qualified Server.List.Router as List ( router )

router :: Router
router = do
    get "/" ~> get "/index.html"
    get "/sample" sample

    mount "/csv" Csv.router
    mount "/list" List.router

    get "/redirect" ~> get "/"

    {- static files -}
    get "/dist/*" $ static "client/dist/"
    get "/*" $ static "client/public/"

root = responder router
