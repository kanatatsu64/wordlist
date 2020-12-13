{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.Cards (
    addHandler,
    deleteHandler
) where

import Prelude hiding ( lookup )

import Convertible ( ConvertError (..), failConvert )
import Server.Bundle ( addCards, deleteCards )
import Server.Response ( ok )
import Server.Handler ( handler )
import Server.Json ( Json (..), Ary (..), parse )
import Server.Types ( lazyDecode, lookup )
import Server.Utils ( body )

addHandler = handler $ \params request -> do
    _bundleid <- lookup "id" params
    bundleid <- failConvert _bundleid
    Ary cardids <- flip body request $ \bstr ->
        let json = Json $ lazyDecode bstr in
            case parse json of
                Right ids -> return ids
                Left error -> fail $ convErrorMessage error
    addCards bundleid cardids
    ok

deleteHandler = handler $ \params request -> do
    _bundleid <- lookup "id" params
    bundleid <- failConvert _bundleid
    Ary cardids <- flip body request $ \bstr ->
        let json = Json $ lazyDecode bstr in
            case parse json of
                Right ids -> return ids
                Left error -> fail $ convErrorMessage error
    deleteCards bundleid cardids
    ok
