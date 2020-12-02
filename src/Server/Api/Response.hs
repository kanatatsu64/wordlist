module Server.Api.Response (
    card,
    bundle
) where

import Network.Wai ( Response )

import Card ( Card )
import Bundle ( Bundle )
import Server.Json ( Json (..) )
import Server.Response ( json )
import Server.Api.Types ( convertCard, convertBundle )

card :: Card -> IO Response
card = json . jsonify . convertCard

bundle :: Bundle -> IO Response
bundle = json . jsonify . convertBundle
