{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Base (
    getPluginById
) where

import Control.Applicative

import Plugin ( PluginID, Plugin (..) )
import Utils ( maybeToFail )
import qualified Plugins.German.Base as German

plugins :: MonadFail m => [m Plugin]
plugins = [
        German.getPlugin
    ]

getPluginById :: MonadFail m => PluginID -> m Plugin
getPluginById = maybeToFail "plugin is not found" . _getPluginById

_getPluginById :: PluginID -> Maybe Plugin
_getPluginById _pluginid = foldl (<|>) Nothing do
    mplugin <- plugins
    return $ when (pluginid <$> mplugin) mplugin _pluginid
    where
        when m a x = do
            y <- m
            if x == y
            then a
            else Nothing
