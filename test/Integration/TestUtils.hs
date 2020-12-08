{-# LANGUAGE RankNTypes #-}

module Integration.TestUtils (
    log,
    execRuntime
) where

import Prelude hiding ( log )

import Integration.TestConfig ( database )
import Server.SQL ( IConnection, Runtime, execRuntimeOn )

log :: String -> IO ()
log = putStr . show

execRuntime :: (forall c. IConnection c => Runtime c a) -> IO a
execRuntime = execRuntimeOn database
