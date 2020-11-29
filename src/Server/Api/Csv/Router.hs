{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Csv.Router (
    router
) where

import Prelude hiding ( lookup )

import CardClass ( fromCsv )
import Directory ( doesFileExist )
import qualified German.Base as German ( Card )
import Server.Json ( Json (..) )
import Server.Response ( json, notFound )
import Server.Handler ( handler )
import Server.Types ( lookup, decode )
import Server.Internal.Router ( get, (~>) )
import Server.Api.Csv.Types ( Csv (..), convertCard, replaceId )

import qualified Server.Api.Csv.List as List ( getCSVHandler, getNameHandler )

router = do
    get ":name" getHandler
    get "list" ~> get "list/csv"
    get "list/csv" List.getCSVHandler
    get "list/name" List.getNameHandler

getHandler = handler $ \params -> do
    case lookup "name" params of
        Just _name -> do
            let name = decode _name
            let path = "resource/" <> name <> ".csv"
            exist <- doesFileExist path
            if exist
            then do
                _cards <- fromCsv @German.Card path
                cards <- mapM (replaceId . convertCard) _cards
                let csv = Csv name cards
                json $ jsonify csv
            else notFound
