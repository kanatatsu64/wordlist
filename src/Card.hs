{-# LANGUAGE FlexibleContexts #-}

module Card (
    Card (..),
    CardID,
    Language (..),
    Note,
    Example (..),
    CardSchema,
    AttrSchema,
    ExampleSchema,
    cardTable,
    attrTable,
    exampleTable,
    runSave,
    runLoad,
    save,
    load
) where

import Prelude hiding ( lookup )
import Control.Monad
import Data.Sort ( sortOn )

import Html (Htmlizable (..), Html (..), TagName (..), Content (..) )
import Serializable ( Serializable (..), Serial (..) )
import Composable ( Composable (..) )
import SQL (
        IConnection,
        ISchema (..),
        execRuntime,
        Runtime,
        Table,
        columns,
        lookup,
        toSql,
        runInsert,
        runSelect
    )
import Utils ( withIndex, same, for, maybeToFail )
import Types ( CardID, Language (..), Note, Example (..), PluginID )

data Card = Card {
    cardid :: CardID,
    pluginid :: PluginID,
    language :: Language,
    word :: String,
    meaning :: String,
    attrs :: [Serial],
    note :: Note,
    examples :: [Example]
}

cardTable :: Table
cardTable = "Card"

attrTable :: Table
attrTable = "Attr"

exampleTable :: Table
exampleTable = "Example"

data CardSchema = CardSchema {
    cs_cardid :: CardID,
    cs_pluginid :: PluginID,
    cs_language :: Language,
    cs_word :: String,
    cs_meaning :: String,
    cs_note :: Note
}

instance ISchema CardSchema where
    table = const cardTable
    definitions = const [
            ("id", "String"),
            ("pluginid", "String"),
            ("language", "String"),
            ("word", "String"),
            ("meaning", "String"),
            ("note", "String")
        ]
    toRecords schema = [
            ("id", toSql $ serialize $ cs_cardid schema),
            ("pluginid", toSql $ serialize $ cs_pluginid schema),
            ("language", toSql $ serialize $ cs_language schema),
            ("word", toSql $ cs_word schema),
            ("meaning", toSql $ cs_meaning schema),
            ("note", toSql $ cs_note schema)
        ]
    fromRecords recs = do
            _cardid <- compose =<< lookup "id" recs
            _pluginid <- compose =<< lookup "pluginid" recs
            _language <- compose =<< lookup "language" recs
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
            ("cardid", "String"),
            ("seq", "String"),
            ("attr", "String")
        ]
    toRecords schema = [
            ("cardid", toSql $ serialize $ as_cardid schema),
            ("seq", toSql $ as_seq schema),
            ("attr", toSql $ serialize $ as_attr schema)
        ]
    fromRecords recs = do
            _cardid <- compose =<< lookup "cardid" recs
            _seq <- lookup "seq" recs
            _attr <- compose =<< lookup "attr" recs
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
            ("cardid", "String"),
            ("original", "String"),
            ("translation", "String")
        ]
    toRecords schema = [
            ("cardid", toSql $ serialize $ es_cardid schema),
            ("original", toSql $ es_original schema),
            ("translation", toSql $ es_translation schema)
        ]
    fromRecords recs = do
            _cardid <- compose =<< lookup "cardid" recs
            _original <- lookup "original" recs
            _translation <- lookup "translation" recs
            return $ ExampleSchema _cardid _original _translation

toExampleSchemas :: Card -> [ExampleSchema]
toExampleSchemas card = do
        Example _original _translation <- _examples
        return $ ExampleSchema _cardid _original _translation
    where _cardid = cardid card
          _examples = examples card

fromSchemas :: CardSchema -> [AttrSchema] -> [ExampleSchema] -> Maybe Card
fromSchemas cardSchema attrSchemas exampleSchemas = do
    let _cs_cardid = cs_cardid cardSchema
        _as_cardids = map as_cardid attrSchemas
        _es_cardids = map es_cardid exampleSchemas
    guard $ same $ [_cs_cardid] <> _as_cardids <> _es_cardids
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

runLoad :: IConnection conn => CardID -> Runtime conn Card
runLoad _cardid = do
        let val = toSql $ serialize _cardid
        crs <- runSelect cardTable (columns @CardSchema undefined) ("id = ?", [val])
        css <- mapM (maybeToFail "failed to parse Card table" . fromRecords) crs
        cards <- sequence do
            cs <- css
            return do
                ars <- runSelect attrTable (columns @AttrSchema undefined) ("cardid = ?", [val])
                ass <- mapM (maybeToFail "failed to parse Attr table" . fromRecords) ars
                ers <- runSelect exampleTable (columns @ExampleSchema undefined) ("cardid = ?", [val])
                ess <- mapM (maybeToFail "failed to parse Example table" . fromRecords) ers
                maybeToFail "failed to compose Card" $ fromSchemas cs ass ess
        case length cards of
            1 -> return $ head cards
            0 -> fail "Card is not found"
            _ -> fail "multiple Cards are found"

load :: CardID -> IO Card
load _cardid = execRuntime $ runLoad _cardid

_toHtml card = Tag TD [] [
        Child $ Tag TD [] [Text $ serialize $ language card],
        Child $ Tag TD [] [Text $ word card],
        Child $ Tag TD [] [Text $ meaning card]
    ]

instance Htmlizable Card where
    toHtml = _toHtml
