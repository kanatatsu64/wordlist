module Server.Csv.Router (
    router
) where

import Server.Internal.Router ( get, post )

import qualified Server.Csv.Upload as Upload ( getHandler, postHandler )

router = do
    get "/csv/upload" Upload.getHandler
    post "/csv/upload" Upload.postHandler
