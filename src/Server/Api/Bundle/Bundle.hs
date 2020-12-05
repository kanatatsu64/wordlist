{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.Bundle (
    createHandler,
    getHandler,
    uploadHandler
) where

import Prelude hiding ( lookup )
import Network.Wai.Parse ( FileInfo (..) )

import Csv ( parseCsv )
import Bundle ( Bundle (..), save, load, runAddCards )
import Composable ( Composable (..) )
import SQL ( execRuntime )
import Plugins.Base ( getPluginById )
import UUID ( getRandom )
import Utils ( maybeToFail )
import qualified Card ( Card (..), runSave )
import Server.Types ( decode, lazyDecode, lookup )
import Server.Handler ( handler )
import Server.Response ( uploader, ok )
import Server.Api.Response ( bundle )

createHandler = handler $ \params -> do
    _name <- lookup "name" params
    let name = decode _name
    _desc <- lookup "desc" params
    let desc = decode _desc
    uuid <- getRandom
    let _bundle = Bundle uuid name desc []
    save _bundle
    ok

getHandler = handler $ \params -> do
    _bundleid <- lookup "id" params
    bundleid <- maybeToFail "failed to compose bundleid" $ compose $ decode _bundleid
    _bundle <- load bundleid
    bundle _bundle

uploadHandler = handler $ \params request -> do
    _bundleid <- lookup "id" params
    bundleid <- maybeToFail "failed to compose bundleid" $ compose $ decode _bundleid
    _bundle <- load bundleid
    _pluginid <- lookup "plugin" params
    pluginid <- maybeToFail "failed to compose pluginid" $ compose $ decode _pluginid
    _plugin <- getPluginById pluginid
    flip uploader request \(_, info) -> do
        let contents = lazyDecode $ fileContent info
        cards <- parseCsv _plugin (lines contents)
        execRuntime do
            mapM_ Card.runSave cards
            runAddCards _bundle (map Card.cardid cards)
