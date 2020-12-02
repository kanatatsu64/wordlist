{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.Router (
    router
) where

import Prelude hiding ( lookup )

import Csv ( loadCsv )
import Bundle ( Bundle (..) )
import UUID ( fromString )
import Directory ( doesFileExist )
import qualified Plugins.German.Base as German ( getPlugin )
import Server.Response ( notFound )
import Server.Handler ( handler )
import Server.Types ( lookup, decode )
import Server.Internal.Router ( get, (~>) )
import Server.Api.Response ( bundle )

import qualified Server.Api.Bundle.List as List ( getList, getNameList )

router = do
    get "name/:name" getHandler
    get "list" List.getList
    get "list/name" List.getNameList

getHandler = handler $ \params -> do
    _name <- lookup "name" params
    let name = decode _name
    let path = "resource/" <> name <> ".csv"
    exist <- doesFileExist path
    if exist
    then do
        plugin <- German.getPlugin
        cards <- loadCsv plugin path
        bundle $ Bundle name "" cards
    else notFound
