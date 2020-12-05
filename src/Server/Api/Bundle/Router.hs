module Server.Api.Bundle.Router (
    router
) where

import Server.Internal.Router ( get, post )

import qualified Server.Api.Bundle.List as List ( getNameList )
import Server.Api.Bundle.Bundle ( createHandler, getHandler, uploadHandler )

router = do
    post "" createHandler
    get ":id" getHandler
    post ":id" uploadHandler
    get "list/name" List.getNameList
