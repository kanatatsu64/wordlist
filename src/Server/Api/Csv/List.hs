{-# LANGUAGE TypeApplications #-}

module Server.Api.Csv.List (
    getCSVHandler,
    getNameHandler
) where

import Prelude hiding ( Word )

import Control.Monad

import Directory ( listCsvs, listCsvNames, getName, delExt )
import CardClass ( fromCsv )
import qualified German.Base as German ( Card )

import Server.Json ( Json (..), Ary (..) )
import Server.Handler ( handler )
import Server.Response ( json )
import Server.Api.Csv.Types ( Csv (..), convertCard, replaceId )

getCSVHandler = handler $ do
    paths <- listCsvs "resource/"
    csvs <- forM paths $ \path -> do
        _cards <- fromCsv @German.Card path
        let name = delExt $ getName path
        cards <- mapM (replaceId . convertCard) _cards
        return $ Csv name cards
    json $ jsonify $ Ary csvs

getNameHandler = handler $ do
    names <- map delExt <$> listCsvNames "resource/"
    json $ jsonify $ Ary names
