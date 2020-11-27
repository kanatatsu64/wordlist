{-# LANGUAGE OverloadedStrings #-}

module Server.List.Show (
    index,
    show
) where

import Prelude hiding ( lookup, error, show )

import Server.Types ( lookup, decode )
import Server.Response ( html, error )
import Server.Handler ( handler, getParams )

import Directory ( listCsvNames, delExt )
import Html ( export, template, TagName (..), Html (..), Content (..) )

import Examples.ServerResponse ( list )

index = handler $ do
    csvs <- map delExt <$> listCsvNames "resource/"
    let rows = [Tag LI [] [Child $ Tag A ["href=list/" ++ csv] [Text csv]] | csv <- csvs]
    let inner = Tag UL [] $ map Child rows
    html $ (export . template) inner

show = handler $ do
    params <- getParams
    case lookup "name" params of
        Just name -> return $ list (decode name) "" $ "resource/" ++ decode name ++ ".csv"
        Nothing -> return $ error "name is not given"
