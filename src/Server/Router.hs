{-# LANGUAGE OverloadedStrings #-}

module Server.Router (
    root
) where

import Server.Internal.Router ( Router, responder, get )
import Server.Handler ( sample, static, htmlFile )
import qualified Server.Csv.Router as Csv ( router )
import qualified Server.List.Router as List ( router )

root = responder router

router :: Router
router = do
    get "/" $ htmlFile "client/public/index.html"
    get "/sample" sample
    Csv.router
    List.router

    get "/dist/*" $ static "client/dist/"
    get "/*" $ static "client/public/"
