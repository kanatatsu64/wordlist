module Server.Csv.Router (
    router
) where

import Server.Internal.Router ( get, post )

import qualified Server.Csv.Upload as Upload ( getHandler, postHandler )

router = do
    get "upload" Upload.getHandler
    post "upload" Upload.postHandler
