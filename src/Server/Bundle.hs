module Server.Bundle (
    BundleInfo,
    BundleSchema,
    BundleToCardSchema,
    bundleTable,
    bundleToCardTable,
    runSave,
    runLoad,
    runLoadInfos,
    runAddCard,
    runAddCards,
    save,
    load,
    loadInfos,
    addCard,
    addCards,
    module Bundle
) where

import Prelude hiding ( lookup )
import Control.Monad

import Bundle
import Card ( Card (..), CardID )
import qualified Server.Card as Card
import Server.SQL (
        IConnection,
        ISchema (..),
        Table,
        Runtime,
        uuidDataType,
        columns,
        lookup,
        execRuntime,
        toSql,
        runInsert,
        runSelect,
        runSelectAll
    )
import Utils ( maybeToFail, same, contentEqual )

bundleTable :: Table
bundleTable = "Bundle"

bundleToCardTable :: Table
bundleToCardTable = "BundleToCard"

data BundleSchema = BundleSchema {
    bs_bundleid :: BundleID,
    bs_name :: String,
    bs_desc :: String
}

instance ISchema BundleSchema where
    table = const bundleTable
    definitions = const [
            ("id", uuidDataType),
            ("name", "String"),
            ("desc", "String")
        ]
    toRecords schema = [
            ("id", toSql $ bs_bundleid schema),
            ("name", toSql $ bs_name schema),
            ("desc", toSql $ bs_desc schema)
        ]
    fromRecords recs = do
            _bundleid <- lookup "id" recs
            _name <- lookup "name" recs
            _desc <- lookup "desc" recs
            return $ BundleSchema _bundleid _name _desc

toBundleSchema :: Bundle -> BundleSchema
toBundleSchema bundle = BundleSchema _bundleid _name _desc
    where _bundleid = bundleid bundle
          _name = name bundle
          _desc = desc bundle

data BundleToCardSchema = BundleToCardSchema {
    bcs_bundleid :: BundleID,
    bcs_cardid :: CardID
}

instance ISchema BundleToCardSchema where
    table = const bundleToCardTable
    definitions = const [
            ("bundleid", uuidDataType),
            ("cardid", uuidDataType)
        ]
    toRecords schema = [
            ("bundleid", toSql $ bcs_bundleid schema),
            ("cardid", toSql $ bcs_cardid schema)
        ]
    fromRecords recs = do
            _bundleid <- lookup "bundleid" recs
            _cardid <- lookup "cardid" recs
            return $ BundleToCardSchema _bundleid _cardid

toBundleToCardSchemas :: Bundle -> [BundleToCardSchema]
toBundleToCardSchemas bundle = do
    let _bundleid = bundleid bundle
    card <- cards bundle
    let _cardid = cardid card
    return $ BundleToCardSchema _bundleid _cardid

fromSchemas :: MonadFail m => BundleSchema -> [BundleToCardSchema] -> [Card] -> m Bundle
fromSchemas bundleSchema bundleToCardSchemas cards = do
    let _bs_bundleid = bs_bundleid bundleSchema
        _bcs_bundleids = map bcs_bundleid bundleToCardSchemas
    maybeToFail "bundle ids are not consistent" (
            guard $ same $ [_bs_bundleid] <> _bcs_bundleids
        )
    let _bcs_cardids = map bcs_cardid bundleToCardSchemas
        _cardids = map cardid cards
    maybeToFail "card ids are not consistent" (
            guard $ contentEqual _bcs_cardids _cardids
        )
    let _bundleid = bs_bundleid bundleSchema
        _name = bs_name bundleSchema
        _desc = bs_desc bundleSchema
    return $ Bundle _bundleid _name _desc cards

runSave :: IConnection conn => Bundle -> Runtime conn ()
runSave bundle = do
        let bundleSchema = toBundleSchema bundle
            bundleToCardSchemas = toBundleToCardSchemas bundle
        runInsert bundleTable (toRecords bundleSchema)
        sequence_ do
            schema <- bundleToCardSchemas
            return $ runInsert bundleToCardTable (toRecords schema)
        sequence_ do
            card <- cards bundle
            return $ Card.runSave card

save :: Bundle -> IO ()
save bundle = execRuntime $ runSave bundle

runLoad :: IConnection conn => BundleID -> Runtime conn Bundle
runLoad _bundleid = do
        let val = toSql _bundleid
        brs <- runSelect bundleTable (columns @BundleSchema undefined) ("id = ?", [val])
        bss <- mapM (maybeToFail "failed to parse Bundle Table" . fromRecords) brs
        bcrs <- runSelect bundleToCardTable (columns @BundleToCardSchema undefined) ("bundleid = ?", [val])
        bcss <- mapM (maybeToFail "failed to parse BundleToCard Table" . fromRecords) bcrs
        bundles <- sequence do
            bs <- bss
            return do
                _cards <- sequence do
                    bcs <- bcss
                    let _cardid = bcs_cardid bcs
                    return $ Card.runLoad _cardid
                fromSchemas bs bcss _cards
        case length bundles of
            1 -> return $ head bundles
            0 -> fail "Bundle is not found"
            _ -> fail "multiple Bundles are found"

load :: BundleID -> IO Bundle
load _bundleid = execRuntime $ runLoad _bundleid

type BundleInfo = (BundleID, String, String)

runLoadInfos :: IConnection conn => Runtime conn [BundleInfo]
runLoadInfos = do
        recss <- runSelectAll bundleTable ["id", "name", "desc"]
        forM recss $ \recs -> do
            id <- maybeToFail "id is not found" $ lookup "id" recs
            name <- maybeToFail "name is not found" $ lookup "name" recs
            desc <- maybeToFail "desc is not found" $ lookup "desc" recs
            return (id, name, desc)

loadInfos :: IO [BundleInfo]
loadInfos = execRuntime runLoadInfos

runAddCard :: IConnection conn => Bundle -> CardID -> Runtime conn ()
runAddCard bundle _cardid = do
        let _bundleid = bundleid bundle
            schema = BundleToCardSchema _bundleid _cardid
        void $ runInsert bundleToCardTable (toRecords schema)

runAddCards :: IConnection conn => Bundle -> [CardID] -> Runtime conn ()
runAddCards bundle _cardids = sequence_ do
        _cardid <- _cardids
        return $ runAddCard bundle _cardid

addCard :: Bundle -> CardID -> IO ()
addCard bundle _cardid = execRuntime $ runAddCard bundle _cardid

addCards :: Bundle -> [CardID] -> IO ()
addCards bundle _cardids = execRuntime $ runAddCards bundle _cardids