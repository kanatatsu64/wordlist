{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Internal.Handler (
    Handler (..),
    Controller,
    Handlable (..),
    Env (..),
    gets,
    getEnv,
    getBody,
    getParams,
    getRequest
) where

import Control.Monad.Reader
import Network.Wai ( Request, Response )

import Server.Types ( Body, Param )

data Handler = forall h. Handlable h => Handler h

data Env = Env {
    body :: Body,
    params :: [Param],
    request :: Request
}

type HandlerDSL a = Env -> a

buildHandler :: HandlerDSL a -> Env -> a
buildHandler = id

getEnv :: HandlerDSL Env
getEnv = id

gets :: (Env -> a) -> HandlerDSL a
gets = id

getBody :: HandlerDSL Body
getBody = gets body

getParams :: HandlerDSL [Param]
getParams = gets params

getRequest :: HandlerDSL Request
getRequest = gets request

type Controller = Body -> [Param] -> Request -> IO Response

class Handlable h where
    toController :: h -> Controller

instance Handlable Controller where
    toController = id

instance Handlable (HandlerDSL (IO Response)) where
    toController m = \body params request ->
        buildHandler m $ Env body params request

{-
    instance Handlable (IO Controller) where
        toController m = \request body params -> do
            h <- m
            h body params request

    instance Handlable (Body -> Request -> Response) where
        toController h = \body _ request -> return $ h body request

    instance Handlable ([Param] -> Request -> Response) where
        toController h = \_ params request -> return $ h params request

    instance Handlable (Body -> [Param] -> Response) where
        toController h = \body params _ -> return $ h body params

    instance Handlable (Request -> Response) where
        toController h = \_ _ request -> return $ h request

    instance Handlable (Body -> Response) where
        toController h = \body _ _ -> return $ h body

    instance Handlable ([Param] -> Response) where
        toController h = \_ params _ -> return $ h params

    instance Handlable Response where
        toController h = \_ _ _ -> return h
-}

instance Handlable (Body -> Request -> IO Response) where
    toController h = \body _ request -> h body request

instance Handlable ([Param] -> Request -> IO Response) where
    toController h = \_ params request -> h params request

instance Handlable (Body -> [Param] -> IO Response) where
    toController h = \body params _ -> h body params

instance Handlable (Request -> IO Response) where
    toController h = \_ _ request -> h request

instance Handlable (Body -> IO Response) where
    toController h = \body _ _ -> h body

instance Handlable ([Param] -> IO Response) where
    toController h = \_ params _ -> h params

instance Handlable (IO Response) where
    toController h = \_ _ _ -> h
