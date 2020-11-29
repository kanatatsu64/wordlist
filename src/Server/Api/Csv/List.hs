{-# LANGUAGE TypeApplications #-}

module Server.Api.Csv.List (
    getHandler
) where

import Prelude hiding ( Word )

import Control.Monad

import Server.Json ( Json (..), Ary (..) )
import Directory ( listCsvs, getName, delExt )
import CardClass ( fromCsv )
import qualified German.Base as German ( Card )

import Server.Handler ( handler )
import Server.Response ( json )
import Server.Api.Csv.Types ( Csv (..), convertCardToWord )

getHandler = handler $ do
    paths <- listCsvs "resource/"
    csvs <- forM paths $ \path -> do
        cards <- fromCsv @German.Card path
        let name = delExt $ getName path
        let words = map convertCardToWord cards
        return $ Csv name words
    json $ jsonify $ Ary csvs
