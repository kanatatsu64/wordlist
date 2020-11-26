{-# LANGUAGE FlexibleInstances #-}

module Server.Handler (
    Handler,
    Handlable (..)
) where

import Network.Wai ( Response )

import Server.Types ( Body, Param )

type Handler = Body -> [Param] -> IO Response

class Handlable h where
    toHandler :: h -> Handler

instance Handlable Handler where
    toHandler = id

instance Handlable (IO Handler) where
    toHandler m = \body params -> do
        h <- m
        h body params

instance Handlable ([Param] -> Response) where
    toHandler h = \_ params -> return $ h params

instance Handlable ([Param] -> IO Response) where
    toHandler h = \_ params -> h params

instance Handlable Response where
    toHandler h = \_ _ -> return h

instance Handlable (IO Response) where
    toHandler h = \_ _ -> h
