{-# LANGUAGE ExistentialQuantification #-}

module Env (
    AppEnv (..),
    envRef,
    getsEnv,
    modifyEnv,
    withEnv,
    contEnv
) where

import System.IO.Unsafe
import Control.Concurrent
import Database.HDBC

import Utils ( contT )

data AppEnv = forall conn. IConnection conn => AppEnv {
    env_conn :: conn
}

{-# NOINLINE envRef #-}
envRef :: MVar AppEnv
envRef = unsafePerformIO newEmptyMVar

getsEnv :: (AppEnv -> a) -> IO a
getsEnv f = do
    config <- readMVar envRef
    return $ f config

modifyEnv :: (AppEnv -> AppEnv) -> IO ()
modifyEnv f = modifyMVar envRef (\env -> return (f env, ()))

withEnv :: (AppEnv -> IO a) -> IO a
withEnv callback = do
    env <- takeMVar envRef
    res <- callback env
    putMVar envRef env
    return res

contEnv = contT withEnv
