module Server.Api.Plugin.Router (
    router
) where

import Server.Internal.Router ( get )

import qualified Server.Api.Plugin.Info as Info ( getHandler )

router = do
    get "info/:id" Info.getHandler
