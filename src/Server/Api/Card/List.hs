{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Card.List (
    uploadHandler
) where

import Prelude hiding ( lookup )
import Control.Monad
import Network.Wai.Parse ( FileInfo (..) )

import Csv ( parseCsv )
import Convertible ( failConvert )
import Plugins.Base ( getPluginById )
import Server.Card ( Card (..), runSave )
import Server.SQL ( execRuntime )
import Server.Json ( jsonify, Ary (..) )
import Server.Types ( decode, lazyDecode, lookup )
import Server.Handler ( handler )
import Server.Response ( json )
import Server.Utils ( upload )

uploadHandler = handler $ \params request -> do
    _pluginid <- lookup "plugin" params
    pluginid <- failConvert $ decode _pluginid
    _plugin <- getPluginById pluginid
    cards <- join <$> flip upload request \(_, info) -> do
        let contents = lazyDecode $ fileContent info
        cards <- parseCsv _plugin (lines contents)
        execRuntime do
            mapM_ runSave cards
            return cards
    json $ jsonify $ Ary (map cardid cards)
