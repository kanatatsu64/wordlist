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
import System.Directory ( doesFileExist )

index = handler $ do
    csvs <- map delExt <$> listCsvNames "resource/"
    let rows = [Tag LI [] [Child $ Tag A ["href=list/" ++ csv] [Text csv]] | csv <- csvs]
    let inner = Tag UL [] $ map Child rows
    html $ (export . template) inner

show = handler $ do
    params <- getParams
    case lookup "name" params of
        Just name -> return $ do
            let path = "resource/" ++ decode name ++ ".csv"
            exist <- doesFileExist path
            if exist
            then list (decode name) "" path
            else error "Such bundle is not found"
        Nothing -> return $ error "name is not given"
