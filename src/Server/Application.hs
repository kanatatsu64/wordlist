module Server.Application (
    app
) where

import Network.Wai ( Application )

import Server.Router ( root )

app :: Application
app request respond = respond =<< root request
