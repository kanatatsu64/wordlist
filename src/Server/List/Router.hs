module Server.List.Router (
    router
) where

import Server.Internal.Router ( get )
import qualified Server.List.Show as Show ( getHandler )

router = do
    get "/list/:name" Show.getHandler
