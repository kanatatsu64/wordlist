module Plugin (
    PluginID,
    Plugin (..)
) where

import Card ( Card )
import Types ( CardID, PluginID )

data Plugin = Plugin {
    pluginid :: PluginID,
    name :: String,
    desc :: String,
    toCard :: CardID -> [String] -> Maybe Card,
    fromCard :: Card -> [String]
}
