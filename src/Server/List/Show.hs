{-# LANGUAGE OverloadedStrings #-}

module Server.List.Show (
    getHandler
) where

import Prelude hiding ( lookup, error )

import Server.Types ( lookup, decode )
import Server.Response ( error )
import Server.Handler ( handler, getParams )

import Examples.ServerResponse ( list )

getHandler = handler $ do
    params <- getParams
    case lookup "name" params of
        Just name -> return $ list (decode name) "" $ "resource/" ++ decode name ++ ".csv"
        Nothing -> return $ error "name is not given"
