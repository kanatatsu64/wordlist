module Server.Api.Card.Router (
    router
) where

import Server.Internal.Router ( get, post, put, delete )
import Server.Api.Card.Card ( getHandler, createHandler, updateHandler, deleteHandler )
import qualified Server.Api.Card.List as List ( uploadHandler )

router = do
    post "" createHandler
    put "" updateHandler
    post "list" List.uploadHandler
    get ":id" getHandler
    delete ":id" deleteHandler
