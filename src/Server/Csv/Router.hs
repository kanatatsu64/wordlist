module Server.Csv.Router (
    router
) where

import Server.Internal.Router ( get, post, (~>) )

import qualified Server.Csv.Upload as Upload ( postHandler )

router = do
    get "upload" ~> get "csv.html"
    post "upload" Upload.postHandler
