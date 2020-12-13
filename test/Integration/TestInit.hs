module TestInit (
    initTest
) where

import Control.Monad

import Config ( AppConfig (..), modifyConfig )
import TestConfig ( database )
import Utils ( field )
import Server.SQL ( execRuntime, runExistTable, runCreateTable, ISchema (..), IConnection, Runtime )
import Server.Bundle ( BundleSchema, BundleToCardSchema )
import Server.Card ( CardSchema, AttrSchema, ExampleSchema )

initTest :: IO ()
initTest = do
    initConfig
    initDB

initConfig :: IO ()
initConfig = do
    modifyConfig (\config -> config { cf_database = database })

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
