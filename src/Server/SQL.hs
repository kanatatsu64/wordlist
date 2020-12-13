{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Server.SQL (
    withConnection,
    contConnection,
    execRuntime,
    uuidDataType,
    serialDataType,
    module Server.Internal.SQL
) where

import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )

import Config ( AppConfig (..), getsConfig )
import Convertible ( Convertible (..) )
import UUID ( UUID )
import Serial ( Serial )
import Utils ( execCont, cont )
import Server.Internal.SQL

connect :: FilePath -> IO Connection
connect = connectSqlite3

withConnection :: FilePath ->
                  (Connection -> IO a) ->
                  IO a
withConnection path callback = do
    connection <- connect path
    result <- callback connection
    disconnect connection
    return result

contConnection path = cont $ withConnection path

execRuntime :: (forall c. IConnection c => Runtime c a) -> IO a
execRuntime runtime = do
    db <- getsConfig cf_database
    execCont do
        conn <- contConnection db
        trans <- contTransaction conn
        return $ runRuntime runtime trans

uuidDataType :: DataType
uuidDataType = "String"

instance Convertible UUID SqlValue where
    safeConvert uuid = safeConvert uuid >>= safeConvert @String

instance Convertible SqlValue UUID where
    safeConvert sval = safeConvert sval >>= safeConvert @String

serialDataType :: DataType
serialDataType = "String"

instance Convertible SqlValue Serial where
    safeConvert sval = safeConvert sval >>= safeConvert @String

instance Convertible Serial SqlValue where
    safeConvert sval = safeConvert sval >>= safeConvert @String
