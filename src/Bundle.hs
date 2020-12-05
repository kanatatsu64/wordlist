{-# LANGUAGE FlexibleContexts #-}

module Bundle (
    Bundle (..),
    BundleInfo,
    runSave,
    runLoad,
    runLoadInfos,
    save,
    load,
    loadInfos
) where

import Prelude hiding ( lookup )
import Control.Monad

import Html ( Htmlizable (..), TagName (..), Content (..) )
import qualified Html ( Html (..) )
import Card ( Card (..) )
import qualified Card ( runSave, runLoad )
import Serializable ( Serializable (..) )
import Composable ( Composable (..) )
import Types ( BundleID, CardID )
import Utils ( same, maybeToFail )
import SQL (
        IConnection,
        ISchema (..),
        Table,
        Runtime,
        lookup,
        execRuntime,
        toSql,
        runInsert,
        runSelect,
        runSelectAll
    )

data Bundle = Bundle {
    bundleid :: BundleID,
    name :: String,
    desc :: String,
    cards :: [Card]
}

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
    columns = const [
            "id",
            "name",
            "desc"
        ]
    toRecords schema = [
            ("id", toSql $ serialize $ bs_bundleid schema),
            ("name", toSql $ bs_name schema),
            ("desc", toSql $ bs_desc schema)
        ]
    fromRecords recs = do
            _bundleid <- compose =<< lookup "id" recs
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
    columns = const [
            "bundleid",
            "cardid"
        ]
    toRecords schema = [
            ("bundleid", toSql $ serialize $ bcs_bundleid schema),
            ("cardid", toSql $ serialize $ bcs_cardid schema)
        ]
    fromRecords recs = do
            _bundleid <- compose =<< lookup "bundleid" recs
            _cardid <- compose =<< lookup "cardid" recs
            return $ BundleToCardSchema _bundleid _cardid

toBundleToCardSchemas :: Bundle -> [BundleToCardSchema]
toBundleToCardSchemas bundle = do
    let _bundleid = bundleid bundle
    card <- cards bundle
    let _cardid = cardid card
    return $ BundleToCardSchema _bundleid _cardid

fromSchemas :: BundleSchema -> [BundleToCardSchema] -> [Card] -> Maybe Bundle
fromSchemas bundleSchema bundleToCardSchemas cards = do
    let _bs_bundleid = bs_bundleid bundleSchema
        _bcs_bundleids = map bcs_bundleid bundleToCardSchemas
    guard $ same $ [_bs_bundleid] <> _bcs_bundleids
    let _bcs_cardids = map bcs_cardid bundleToCardSchemas
        _cardids = map cardid cards
    guard $ same $ _bcs_cardids <> _cardids
    let _bundleid = bs_bundleid bundleSchema
        _name = bs_name bundleSchema
        _desc = bs_desc bundleSchema
    return $ Bundle _bundleid _name _desc cards

runSave :: IConnection conn => Bundle -> Runtime conn ()
runSave bundle conn = do
        let bundleSchema = toBundleSchema bundle
            bundleToCardSchemas = toBundleToCardSchemas bundle
        runInsert bundleTable (toRecords bundleSchema) conn
        sequence_ do
            schema <- bundleToCardSchemas
            return $ runInsert bundleToCardTable (toRecords schema) conn
        sequence_ do
            card <- cards bundle
            return $ Card.runSave card conn

save :: Bundle -> IO ()
save bundle = execRuntime $ runSave bundle

runLoad :: IConnection conn => BundleID -> Runtime conn Bundle
runLoad _bundleid conn = do
        let val = toSql $ serialize _bundleid
        brs <- runSelect bundleTable (columns @BundleSchema undefined) ("id = ?", [val]) conn
        bss <- mapM (maybeToFail "failed to parse Bundle Table" . fromRecords) brs
        bcrs <- runSelect bundleToCardTable (columns @BundleToCardSchema undefined) ("id = ?", [val]) conn
        bcss <- mapM (maybeToFail "failed to parse BundleToCard Table" . fromRecords) bcrs
        bundles <- sequence do
            bs <- bss
            return do
                _cards <- sequence do
                    bcs <- bcss
                    let _cardid = bcs_cardid bcs
                    return $ Card.runLoad _cardid conn
                maybeToFail "failed to compose Bundle" $ fromSchemas bs bcss _cards
        case length bundles of
            1 -> return $ head bundles
            0 -> fail "Bundle is not found"
            _ -> fail "multiple Bundles are found"

load :: BundleID -> IO Bundle
load _bundleid = execRuntime $ runLoad _bundleid

type BundleInfo = (BundleID, String)

runLoadInfos :: IConnection conn => Runtime conn [BundleInfo]
runLoadInfos conn = do
        recss <- runSelectAll bundleTable ["id", "name"] conn
        forM recss $ \recs -> do
            id <- maybeToFail "id is not found" $ compose =<< lookup "id" recs
            name <- maybeToFail "name is not found" $ lookup "name" recs
            return (id, name)

loadInfos :: IO [BundleInfo]
loadInfos = execRuntime runLoadInfos

{-
    Bundle "german" "from textbook" [..]

    <table>
        <caption> german (from textbook) </caption>
        <tr> .. </tr>
        ...
    </table>
-}

_toHtml (Bundle _ name desc cards) = Html.Tag TABLE [] children
    where caption = Html.Tag CAPTION [] [
                  Text $ name ++ _desc
              ]
          _desc = case desc of
                     "" -> ""
                     _ -> " (" ++ desc ++ ")"
          rows = map toHtml cards
          children = map Child (caption:rows)

instance Htmlizable Bundle where
    toHtml = _toHtml
