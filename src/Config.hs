module Config (
    AppConfig (..),
    database,
    getsConfig,
    modifyConfig
) where

import System.IO.Unsafe
import Server.Internal.SQL ( Database )
import Data.IORef

data AppConfig = AppConfig {
    cf_database :: Database
}

database :: Database
database = "resource/database.db"

defaultConfig :: AppConfig
defaultConfig = AppConfig database

{-# NOINLINE configRef #-}
configRef :: IORef AppConfig
configRef = unsafePerformIO (newIORef defaultConfig)

getsConfig :: (AppConfig -> a) -> IO a
getsConfig f = do
    config <- readIORef configRef
    return $ f config

modifyConfig :: (AppConfig -> AppConfig) -> IO ()
modifyConfig f = modifyIORef configRef f
