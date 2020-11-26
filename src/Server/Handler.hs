{-# LANGUAGE FlexibleInstances #-}

module Server.Handler (
    Handler,
    Handlable (..)
) where

import Network.Wai ( Response )

import Server.Types ( Body, Param )

type Handler = Body -> [Param] -> Response

class Handlable h where
    toHandler :: h -> Handler

instance Handlable Handler where
    toHandler = id

instance Handlable ([Param] -> Response) where
    toHandler h = \_ params -> h params

instance Handlable Response where
    toHandler h = \_ _ -> h
