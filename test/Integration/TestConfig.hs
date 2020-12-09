module TestConfig (
    database
) where

import Server.SQL ( Database )

database :: Database
database = "resource/test.db"
