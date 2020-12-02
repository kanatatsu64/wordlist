{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.List (
    getList,
    getNameList
) where

import Prelude hiding ( Word, lookup )

import Control.Monad

import Directory ( listCsvs, listCsvNames, getName, delExt )
import Csv ( loadCsv )
import Bundle ( Bundle (..) )
import UUID ( fromString )
import qualified Plugins.German.Base as German ( getPlugin )

import Server.Json ( Json (..), Ary (..) )
import Server.Handler ( handler )
import Server.Response ( json )
import Server.Types ( lookup, decode )
import Server.Api.Types ( convertBundle )

getList = handler $ do
    plugin <- German.getPlugin
    paths <- listCsvs "resource/"
    bundles <- forM paths $ \path -> do
        let name = delExt $ getName path
        cards <- loadCsv plugin path
        let bundle = Bundle name "" cards
        return $ convertBundle bundle
    json $ jsonify $ Ary bundles

getNameList = handler $ do
    names <- map delExt <$> listCsvNames "resource/"
    json $ jsonify $ Ary names
