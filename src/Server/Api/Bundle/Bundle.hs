{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.Bundle (
    createHandler,
    updateHandler,
    getHandler,
    deleteHandler
) where

import Prelude hiding ( lookup )

import UUID ( getRandom )
import Convertible ( convert, failConvert, ConvertError (..) )
import Server.Bundle ( Bundle (..), load, delete, runDelete, runSave, runAddCards )
import Server.Json ( Json (..), jsonify, parse )
import Server.SQL ( execRuntime )
import Server.Types ( lookup, lazyDecode )
import Server.Utils ( body )
import Server.Handler ( handler )
import Server.Response ( ok, json )
import Server.Api.Bundle.Types ( AbbrBundle (..) )

createHandler = handler $ \request -> do
    uuid <- getRandom
    abbr <- flip body request $ \bstr ->
        let json = Json $ lazyDecode bstr in
            case parse @AbbrBundle json of
                Right abbr -> return $ abbr {abbr_bundleid = uuid }
                Left error -> fail $ convErrorMessage error
    let bundle = convert abbr
    execRuntime $ do
        runSave bundle
        runAddCards (bundleid bundle) (abbr_cardids abbr)
    json $ jsonify uuid

updateHandler = handler $ \request -> do
    abbr <- flip body request $ \bstr ->
        let json = Json $ lazyDecode bstr in
            case parse @AbbrBundle json of
                Right abbr -> return abbr
                Left error -> fail $ convErrorMessage error
    let bundle = convert abbr
    execRuntime $ do
        runDelete (abbr_bundleid abbr)
        runSave bundle
        runAddCards (bundleid bundle) (abbr_cardids abbr)
    ok

getHandler = handler $ \params -> do
    _bundleid <- lookup "id" params
    bundleid <- failConvert _bundleid
    _bundle <- load bundleid
    json $ jsonify _bundle

deleteHandler = handler $ \params -> do
    _bundleid <- lookup "id" params
    bundleid <- failConvert _bundleid
    delete bundleid
    ok
