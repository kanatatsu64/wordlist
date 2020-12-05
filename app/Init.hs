module Init (
    initApp
) where

import Control.Monad

import SQL ( execRuntime, runExistTable, runCreateTable, ISchema (..), IConnection, Runtime )
import Bundle ( BundleSchema, BundleToCardSchema )
import Card ( CardSchema, AttrSchema, ExampleSchema )

initApp :: IO ()
initApp = initDB

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
