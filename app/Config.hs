module Config (
    database
) where

import Server.SQL ( Database )

database :: Database
database = "resource/database.db"
