module Server.List.Router (
    router
) where

import Server.Internal.Router ( get )
import qualified Server.List.Show as Show ( index, show )

router = do
    get "/list" Show.index
    get "/list/:name" Show.show
