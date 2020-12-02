module Server.Api.Router (
    router
) where

import Server.Internal.Router ( mount )
import qualified Server.Api.Bundle.Router as Bundle ( router )

router = do
    mount "bundle" Bundle.router
