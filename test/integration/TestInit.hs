module TestInit (
    initTest
) where

import Control.Monad
import Control.Concurrent
import Database.HDBC.Sqlite3 ( connectSqlite3 )

import Env ( AppEnv (..), envRef )
import TestConfig ( database )
import Utils ( field )
import Server.SQL ( execRuntime, runExistTable, runCreateTable, ISchema (..), IConnection, Runtime )
import Server.Bundle ( BundleSchema, BundleToCardSchema )
import Server.Card ( CardSchema, AttrSchema, ExampleSchema )

initTest :: IO ()
initTest = do
    initEnv
    initDB

initEnv :: IO ()
initEnv = do
    conn <- connectSqlite3 database
    putMVar envRef (AppEnv conn)

initDB :: IO ()
initDB = execRuntime do
    field @BundleSchema upsert
    field @BundleToCardSchema upsert
    field @CardSchema upsert
    field @AttrSchema upsert
    field @ExampleSchema upsert
    where
        upsert :: (ISchema schema, IConnection conn) => schema -> Runtime conn ()
        upsert schema = do
            exist <- runExistTable (table schema)
            unless exist $ void $ runCreateTable (table schema) (definitions schema)
