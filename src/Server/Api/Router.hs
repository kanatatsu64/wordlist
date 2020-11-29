module Server.Api.Router (
    router
) where

import Server.Internal.Router ( mount )
import qualified Server.Api.Csv.Router as Csv ( router )

router = do
    mount "csv" Csv.router
