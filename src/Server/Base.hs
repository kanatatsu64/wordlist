module Server.Base (
    runServer
) where

import Network.Wai.Handler.Warp ( run )

import Server.Application ( app )

port = 3000

{-
    import Server.Base ( runServer )

    main :: IO ()
    main = runServer
-}
runServer :: IO ()
runServer = do
    putStrLn $ "Listening on port " ++ show port
    run port app
