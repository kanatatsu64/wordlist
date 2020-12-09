{-# LANGUAGE RankNTypes #-}

module TestUtils (
    execRuntime
) where

import TestConfig ( database )
import Server.SQL ( IConnection, Runtime, execRuntimeOn )

execRuntime :: (forall c. IConnection c => Runtime c a) -> IO a
execRuntime = execRuntimeOn database
