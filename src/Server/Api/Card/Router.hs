module Server.Api.Card.Router (
    router
) where

import Server.Internal.Router ( get )
import Server.Api.Card.Card ( getHandler )

router = do
    get ":id" getHandler
