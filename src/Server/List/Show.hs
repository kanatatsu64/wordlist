module Server.List.Show (
    getHandler
) where

import Prelude hiding ( lookup, error )

import Server.Types ( lookup, encode, decode )
import Server.Response ( error )
import Server.Handler ( handler )

import Examples.ServerResponse ( list )

getHandler = handler $ \params -> case lookup (encode "name") params of
    Just name -> list (decode name) "" $ "resource/" ++ decode name ++ ".csv"
    Nothing -> error "name is not given"
