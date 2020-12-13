{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Server.SQL (
    execRuntime,
    uuidDataType,
    serialDataType,
    bundleTable,
    bundleToCardTable,
    cardTable,
    attrTable,
    exampleTable,    
    module Server.Internal.SQL
) where

import Control.Monad.IO.Class

import Env ( AppEnv (..), contEnv )
import Convertible ( Convertible (..) )
import UUID ( UUID )
import Serial ( Serial )
import Utils ( execContT )
import Server.Internal.SQL

{-
   Due to HDBC sqlite3 driver's bug in clone function,
   serialize multiple connections by hand instead of
   using clone.
-}

execRuntime :: (forall c. IConnection c => Runtime c a) -> IO a
execRuntime runtime = execContT do
    AppEnv conn <- contEnv
    trans <- contTransaction conn
    liftIO $ runRuntime runtime trans

{-
execRuntime :: (forall c. IConnection c => Runtime c a) -> IO a
execRuntime runtime = do
    wconn <- execContT do
        AppEnv base <- contEnv
        conn <- liftIO $ clone base
        return $ ConnWrapper conn
    execContT do
        trans <- contTransaction wconn
        liftIO $ runRuntime runtime trans
-}

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

bundleTable :: Table
bundleTable = "Bundle"

bundleToCardTable :: Table
bundleToCardTable = "BundleToCard"

cardTable :: Table
cardTable = "Card"

attrTable :: Table
attrTable = "Attr"

exampleTable :: Table
exampleTable = "Example"
