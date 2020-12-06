module Server.Api.Router (
    router
) where

import Server.Internal.Router ( mount )
import qualified Server.Api.Bundle.Router as Bundle ( router )
import qualified Server.Api.Plugin.Router as Plugin ( router )

router = do
    mount "bundle" Bundle.router
    mount "plugin" Plugin.router
