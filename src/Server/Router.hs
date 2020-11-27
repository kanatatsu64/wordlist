{-# LANGUAGE OverloadedStrings #-}

module Server.Router (
    root
) where

import Server.Internal.Router ( Router, responder, get )
import Server.Handler ( sample, htmlFile )
import qualified Server.Csv.Router as Csv ( router )
import qualified Server.List.Router as List ( router )

root = responder router

router :: Router
router = do
    get "/" $ htmlFile "client/index.html"
    get "/sample" sample
    Csv.router
    List.router
