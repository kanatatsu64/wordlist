module Main where

import Init ( initApp )
import Server.Base ( runServer )

main :: IO ()
main = do
    initApp
    runServer
