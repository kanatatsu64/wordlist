{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Card (
    CardSchema,
    AttrSchema,
    ExampleSchema,
    cardTable,
    attrTable,
    exampleTable,
    runExist,
    runSave,
    runDelete,
    runLoad,
    exist,
    save,
    delete,
    load,
    module Card
) where

import Prelude hiding ( lookup )
import Control.Monad
import Data.Sort ( sortOn )

import Card
import Plugin ( PluginID )
import Serial ( Serial )
import Convertible ( Convertible (..) )
import Server.SQL (
        IConnection,
        ISchema (..),
        uuidDataType,
        serialDataType,
        execRuntime,
        Runtime,
        DataType,
        Table,
        columns,
        lookup,
        SqlValue,
        toSql,
        runInsert,
        runSelect
    )
import qualified Server.SQL as SQL
import Utils ( withIndex, same, for, maybeToFail )

cardTable :: Table
cardTable = "Card"

attrTable :: Table
attrTable = "Attr"

exampleTable :: Table
exampleTable = "Example"

languageDataType :: DataType
languageDataType = "String"

instance Convertible SqlValue Language where
    safeConvert sval = safeConvert sval >>= safeConvert @String

instance Convertible Language SqlValue where
    safeConvert sval = safeConvert sval >>= safeConvert @String

data CardSchema = CardSchema {
    cs_cardid :: CardID,
    cs_pluginid :: PluginID,
    cs_language :: Language,
    cs_word :: String,
    cs_meaning :: String,
    cs_note :: String
}

instance ISchema CardSchema where
    table = const cardTable
    definitions = const [
            ("id", uuidDataType),
            ("pluginid", uuidDataType),
            ("language", languageDataType),
            ("word", "String"),
            ("meaning", "String"),
            ("note", "String")
        ]
    toRecords schema = [
            ("id", toSql $ cs_cardid schema),
            ("pluginid", toSql $ cs_pluginid schema),
            ("language", toSql $ cs_language schema),
            ("word", toSql $ cs_word schema),
            ("meaning", toSql $ cs_meaning schema),
            ("note", toSql $ cs_note schema)
        ]
    fromRecords recs = do
            _cardid <- lookup "id" recs
            _pluginid <- lookup "pluginid" recs
            _language <- lookup "language" recs
            _word <- lookup "word" recs
            _meaning <- lookup "meaning" recs
            _note <- lookup "note" recs
            return $ CardSchema _cardid _pluginid _language _word _meaning _note

toCardSchema :: Card -> CardSchema
toCardSchema card = CardSchema _cardid _pluginid _language _word _meaning _note
    where _cardid = cardid card
          _pluginid = pluginid card
          _language = language card
          _word = word card
          _meaning = meaning card
          _note = note card
          
data AttrSchema = AttrSchema {
    as_cardid :: CardID,
    as_seq :: Integer,
    as_attr :: Serial
}

instance ISchema AttrSchema where
    table = const attrTable
    definitions = const [
            ("cardid", uuidDataType),
            ("seq", "Integer"),
            ("attr", serialDataType)
        ]
    toRecords schema = [
            ("cardid", toSql $ as_cardid schema),
            ("seq", toSql $ as_seq schema),
            ("attr", toSql $ as_attr schema)
        ]
    fromRecords recs = do
            _cardid <- lookup "cardid" recs
            _seq <- lookup "seq" recs
            _attr <- lookup "attr" recs
            return $ AttrSchema _cardid _seq _attr

toAttrSchemas :: Card -> [AttrSchema]
toAttrSchemas card = do
        (index, attr) <- withIndex _attrs
        return $ AttrSchema _cardid index attr
    where _cardid = cardid card
          _attrs = attrs card

data ExampleSchema = ExampleSchema {
    es_cardid :: CardID,
    es_original :: String,
    es_translation :: String
}

instance ISchema ExampleSchema where
    table = const exampleTable
    definitions = const [
            ("cardid", uuidDataType),
            ("original", "String"),
            ("translation", "String")
        ]
    toRecords schema = [
            ("cardid", toSql $ es_cardid schema),
            ("original", toSql $ es_original schema),
            ("translation", toSql $ es_translation schema)
        ]
    fromRecords recs = do
            _cardid <- lookup "cardid" recs
            _original <- lookup "original" recs
            _translation <- lookup "translation" recs
            return $ ExampleSchema _cardid _original _translation

toExampleSchemas :: Card -> [ExampleSchema]
toExampleSchemas card = do
        Example _original _translation <- _examples
        return $ ExampleSchema _cardid _original _translation
    where _cardid = cardid card
          _examples = examples card

fromSchemas :: MonadFail m => CardSchema -> [AttrSchema] -> [ExampleSchema] -> m Card
fromSchemas cardSchema attrSchemas exampleSchemas = do
    let _cs_cardid = cs_cardid cardSchema
        _as_cardids = map as_cardid attrSchemas
        _es_cardids = map es_cardid exampleSchemas
    maybeToFail "card ids are not consistent" (
            guard $ same $ [_cs_cardid] <> _as_cardids <> _es_cardids
        )
    let _cardid = cs_cardid cardSchema
        _pluginid = cs_pluginid cardSchema
        _language = cs_language cardSchema
        _word = cs_word cardSchema
        _meaning = cs_meaning cardSchema
        _attrs = map as_attr $ sortOn as_seq attrSchemas
        _note = cs_note cardSchema
        _examples = for exampleSchemas do
                            _original <- es_original
                            _translation <- es_translation
                            return $ Example _original _translation
    return $ Card _cardid _pluginid _language _word _meaning _attrs _note _examples

runExist :: IConnection conn => CardID -> Runtime conn Bool
runExist _cardid = do
        let val = toSql _cardid
        SQL.runExist cardTable ("id = ?", [val])

exist :: CardID -> IO Bool
exist _cardid = execRuntime $ runExist _cardid

runSave :: IConnection conn => Card -> Runtime conn ()
runSave card = do
        let cardSchema = toCardSchema card
            attrSchemas = toAttrSchemas card
            exampleSchemas = toExampleSchemas card
        runInsert cardTable (toRecords cardSchema)
        sequence_ do
            schema <- attrSchemas
            return $ runInsert attrTable (toRecords schema)
        sequence_ do
            schema <- exampleSchemas
            return $ runInsert exampleTable (toRecords schema)

save :: Card -> IO ()
save card = execRuntime $ runSave card

runDelete :: IConnection conn => CardID -> Runtime conn ()
runDelete _cardid = do
        let val = toSql _cardid
        SQL.runDelete cardTable ("id = ?", [val])
        SQL.runDelete attrTable ("cardid = ?", [val])
        SQL.runDelete exampleTable ("cardid = ?", [val])
        return ()

delete :: CardID -> IO ()
delete _cardid = execRuntime $ runDelete _cardid

runLoad :: IConnection conn => CardID -> Runtime conn Card
runLoad _cardid = do
        let val = toSql _cardid
        crs <- runSelect cardTable (columns @CardSchema undefined) ("id = ?", [val])
        css <- mapM (maybeToFail "failed to parse Card table" . fromRecords) crs
        cards <- sequence do
            cs <- css
            return do
                ars <- runSelect attrTable (columns @AttrSchema undefined) ("cardid = ?", [val])
                ass <- mapM (maybeToFail "failed to parse Attr table" . fromRecords) ars
                ers <- runSelect exampleTable (columns @ExampleSchema undefined) ("cardid = ?", [val])
                ess <- mapM (maybeToFail "failed to parse Example table" . fromRecords) ers
                fromSchemas cs ass ess
        case length cards of
            1 -> return $ head cards
            0 -> fail "Card is not found"
            _ -> fail "multiple Cards are found"

load :: CardID -> IO Card
load _cardid = execRuntime $ runLoad _cardid
