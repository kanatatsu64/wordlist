module Init (
    initApp
) where

import Control.Monad

import Utils ( field )
import Server.SQL ( execRuntime, runExistTable, runCreateTable, ISchema (..), IConnection, Runtime )
import Server.Bundle ( BundleSchema, BundleToCardSchema )
import Server.Card ( CardSchema, AttrSchema, ExampleSchema )

initApp :: IO ()
initApp = initDB

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
