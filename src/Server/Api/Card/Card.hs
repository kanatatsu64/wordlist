{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Card.Card (
    getHandler,
    createHandler,
    updateHandler,
    deleteHandler
) where

import Prelude hiding ( lookup )

import Convertible ( failConvert, ConvertError (..) )
import UUID ( getRandom )
import Server.Card ( Card (..), load, delete, update, save )
import Server.Json ( Json (..), parse, jsonify )
import Server.Types ( decode, lazyDecode, lookup )
import Server.Handler ( Handler, handler )
import Server.Response ( json, ok )
import Server.Utils ( body )

getHandler :: Handler
getHandler = handler $ \params -> do
    _cardid <- lookup "id" params
    cardid <- failConvert $ decode _cardid
    _card <- load cardid
    json $ jsonify _card

createHandler :: Handler
createHandler = handler $ \request -> do
    uuid <- getRandom
    card <- flip body request $ \bstr ->
        let json = Json $ lazyDecode bstr in
            case parse json of
                Right card -> return $ card { cardid = uuid }
                Left error -> fail $ convErrorMessage error
    save card
    json $ jsonify uuid

updateHandler :: Handler
updateHandler = handler $ \request -> do
    card <- flip body request $ \bstr ->
        let json = Json $ lazyDecode bstr in
            case parse json of
                Right card -> return card
                Left error -> fail $ convErrorMessage error
    update card
    ok

deleteHandler :: Handler
deleteHandler = handler $ \params -> do
    _cardid <- lookup "id" params
    cardid <- failConvert $ decode _cardid
    delete cardid
    ok
