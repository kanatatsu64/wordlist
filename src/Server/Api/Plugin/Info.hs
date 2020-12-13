{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Plugin.Info (
    getHandler
) where

import Prelude hiding ( lookup )

import Plugin ( Plugin (..) )
import Convertible ( failConvert )
import Plugins.Base ( getPluginById )
import Server.Json ( jsonify, Dict (..), Rec (..) )
import Server.Response ( json )
import Server.Handler ( Handler, handler )
import Server.Types ( decode, lookup )

getHandler :: Handler
getHandler = handler $ \params -> do
    __pluginid <- lookup "id" params
    _pluginid <- failConvert $ decode __pluginid
    plugin <- getPluginById _pluginid
    json $ jsonify $ Dict [
            Rec "pluginid" $ pluginid plugin,
            Rec "name" $ name plugin,
            Rec "desc" $ desc plugin
        ]
