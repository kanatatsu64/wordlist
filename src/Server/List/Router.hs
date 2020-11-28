module Server.List.Router (
    router
) where

import Server.Internal.Router ( get )
import qualified Server.List.Show as Show ( index, show )

router = do
    get "" Show.index
    get ":name" Show.show
