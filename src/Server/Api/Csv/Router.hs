module Server.Api.Csv.Router (
    router
) where

import Server.Internal.Router ( get )

import qualified Server.Api.Csv.List as List ( getHandler )

router = do
    get "list" List.getHandler
