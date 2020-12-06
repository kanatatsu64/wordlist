{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Plugin.Info (
    getHandler
) where

import Prelude hiding ( lookup )

import Plugin ( Plugin (..) )
import Serializable ( Serializable (..) )
import Composable ( Composable (..) )
import Utils ( maybeToFail )
import Plugins.Base ( getPluginById )
import Server.Json ( Json (..), Dict (..), Rec (..) )
import Server.Response ( json )
import Server.Handler ( Handler, handler )
import Server.Types ( decode, lookup )

getHandler :: Handler
getHandler = handler $ \params -> do
    __pluginid <- lookup "id" params
    _pluginid <- maybeToFail "failed to parse pluginid" $ compose $ decode __pluginid
    plugin <- getPluginById _pluginid
    json $ jsonify $ Dict [
            Rec "pluginid" $ serialize (pluginid plugin),
            Rec "name" $ name plugin,
            Rec "desc" $ desc plugin
        ]
