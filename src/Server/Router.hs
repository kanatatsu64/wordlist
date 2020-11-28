{-# LANGUAGE OverloadedStrings #-}

module Server.Router (
    root
) where

import Server.Internal.Router ( Router, responder, get, mount )
import Server.Handler ( sample, static, htmlFile )
import qualified Server.Csv.Router as Csv ( router )
import qualified Server.List.Router as List ( router )

root = responder router

router :: Router
router = do
    get "/" $ htmlFile "client/public/index.html"
    get "/sample" sample

    mount "/csv/" Csv.router
    mount "/list/" List.router

    {- static files -}
    get "/dist/*" $ static "client/dist/"
    get "/*" $ static "client/public/"
