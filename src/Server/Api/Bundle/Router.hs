module Server.Api.Bundle.Router (
    router
) where

import Server.Internal.Router ( get )

import qualified Server.Api.Bundle.List as List ( getList, getNameList )
import Server.Api.Bundle.Bundle ( getHandler )

router = do
    get "name/:name" getHandler
    get "list" List.getList
    get "list/name" List.getNameList
