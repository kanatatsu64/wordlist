module Integration.TestInit (
    initTest
) where

import Control.Monad

import Server.SQL ( runExistTable, runCreateTable, ISchema (..), IConnection, Runtime )
import Server.Bundle ( BundleSchema, BundleToCardSchema )
import Server.Card ( CardSchema, AttrSchema, ExampleSchema )
import Integration.TestUtils ( execRuntime )

initTest :: IO ()
initTest = initDB

initDB :: IO ()
initDB = execRuntime do
    upsert @BundleSchema undefined
    upsert @BundleToCardSchema undefined
    upsert @CardSchema undefined
    upsert @AttrSchema undefined
    upsert @ExampleSchema undefined
    where
        upsert :: (ISchema schema, IConnection conn) => schema -> Runtime conn ()
        upsert schema = do
            exist <- runExistTable (table schema)
            unless exist $ void $ runCreateTable (table schema) (definitions schema)
