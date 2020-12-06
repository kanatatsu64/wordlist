{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Card.Card (
    getHandler
) where

import Prelude hiding ( lookup )

import Card ( load )
import Composable ( Composable (..) )
import Utils ( maybeToFail )
import Server.Types ( decode, lookup )
import Server.Handler ( Handler, handler )
import Server.Api.Response ( card )

getHandler :: Handler
getHandler = handler $ \params -> do
    _cardid <- lookup "id" params
    cardid <- maybeToFail "failed to compose cardid" $ compose $ decode _cardid
    _card <- load cardid
    card _card
