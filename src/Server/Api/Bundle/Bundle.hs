{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.Bundle (
    getHandler
) where

import Prelude hiding ( lookup )

import Csv ( loadCsv )
import Bundle ( Bundle (..) )
import Directory ( doesFileExist )
import UUID ( getRandom )
import Server.Types ( decode, lookup )
import Server.Handler ( handler )
import Server.Response ( notFound )
import Server.Api.Response ( bundle )
import qualified Plugins.German.Base as German ( getPlugin )

getHandler = handler $ \params -> do
    _name <- lookup "name" params
    let name = decode _name
    let path = "resource/" <> name <> ".csv"
    exist <- doesFileExist path
    if exist
    then do
        plugin <- German.getPlugin
        uuid <- getRandom
        cards <- loadCsv plugin path
        bundle $ Bundle uuid name "" cards
    else notFound
