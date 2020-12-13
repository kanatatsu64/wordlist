module Server.Api.Bundle.Router (
    router
) where

import Server.Internal.Router ( get, post, put, delete )

import qualified Server.Api.Bundle.List as List ( getNameList )
import Server.Api.Bundle.Bundle ( createHandler, updateHandler, getHandler, deleteHandler )

router = do
    post "" createHandler
    put "" updateHandler
    get "list/name" List.getNameList
    get ":id" getHandler
    delete ":id" deleteHandler
