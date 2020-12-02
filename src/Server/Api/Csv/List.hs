module Server.Api.Csv.List (
    getCSVHandler,
    getNameHandler
) where

import Prelude hiding ( Word )

import Control.Monad

import Directory ( listCsvs, listCsvNames, getName, delExt )
import Csv ( loadCsv )
import qualified Plugins.German.Base as German ( getPlugin )

import Server.Json ( Json (..), Ary (..) )
import Server.Handler ( handler )
import Server.Response ( json )
import Server.Api.Csv.Types ( Csv (..), convertCard )

getCSVHandler = handler $ do
    paths <- listCsvs "resource/"
    csvs <- forM paths $ \path -> do
        plugin <- German.getPlugin
        _cards <- loadCsv plugin path
        let name = delExt $ getName path
        let cards = map convertCard _cards
        return $ Csv name cards
    json $ jsonify $ Ary csvs

getNameHandler = handler $ do
    names <- map delExt <$> listCsvNames "resource/"
    json $ jsonify $ Ary names
