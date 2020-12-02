module Server.Api.Handler (
    card,
    bundle
) where

import Card ( Card )
import Bundle ( Bundle )
import Server.Handler ( Handler, handler )
import qualified Server.Api.Response as Response ( card, bundle )

card :: Card -> Handler
card = handler . Response.card

bundle :: Bundle -> Handler
bundle = handler . Response.bundle
