module Plugin (
    PluginID,
    Plugin (..)
) where

import Card ( Card, CardID )
import UUID ( UUID )

type PluginID = UUID

data Plugin = Plugin {
    pluginid :: PluginID,
    toCard :: CardID -> [String] -> Maybe Card,
    fromCard :: Card -> [String]
}
