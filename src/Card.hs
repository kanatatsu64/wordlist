{-# LANGUAGE FlexibleContexts #-}

module Card (
    Card (..),
    CardID,
    Language (..),
    Note,
    Example (..),
    cardTable,
    attrTable,
    exampleTable,
    save
) where

import Control.Monad
import Data.Sort ( sortOn )

import Html (Htmlizable (..), Html (..), TagName (..), Content (..) )
import Serializable ( Serializable (..), Serial (..) )
import Composable ( Composable (..) )
import SQL (
        ISchema (..),
        Table,
        database,
        toSql,
        maybeFromSql,
        contConnection,
        contTransaction,
        runInsert,
        runSelect
    )
import Utils ( withIndex, execCont, same, for, maybeToFail )
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
exampleTable = "Table"

data CardSchema = CardSchema {
    cs_cardid :: CardID,
    cs_pluginid :: PluginID,
    cs_language :: Language,
    cs_word :: String,
    cs_meaning :: String,
    cs_note :: Note
}

instance ISchema CardSchema where
    columns = const [
            "id",
            "pluginid",
            "language",
            "word",
            "meaning",
            "note"
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
            _cardid <- compose =<< get "id" recs
            _pluginid <- compose =<< get "pluginid" recs
            _language <- compose =<< get "language" recs
            _word <- get "word" recs
            _meaning <- get "meaning" recs
            _note <- get "note" recs
            return $ CardSchema _cardid _pluginid _language _word _meaning _note
        where get key recs = maybeFromSql =<< lookup key recs

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
    columns = const [
            "cardid",
            "seq",
            "attr"
        ]
    toRecords schema = [
            ("cardid", toSql $ serialize $ as_cardid schema),
            ("seq", toSql $ as_seq schema),
            ("attr", toSql $ serialize $ as_attr schema)
        ]
    fromRecords recs = do
            _cardid <- compose =<< get "cardid" recs
            _seq <- get "seq" recs
            _attr <- compose =<< get "attr" recs
            return $ AttrSchema _cardid _seq _attr
        where get key recs = maybeFromSql =<< lookup key recs

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
    columns = const [
            "cardid",
            "original",
            "translation"
        ]
    toRecords schema = [
            ("cardid", toSql $ serialize $ es_cardid schema),
            ("original", toSql $ es_original schema),
            ("translation", toSql $ es_translation schema)
        ]
    fromRecords recs = do
            _cardid <- compose =<< get "cardid" recs
            _original <- get "original" recs
            _translation <- get "translation" recs
            return $ ExampleSchema _cardid _original _translation
        where get key recs = maybeFromSql =<< lookup key recs

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

save :: Card -> IO ()
save card = execCont do
    conn <- contConnection database
    trans <- contTransaction conn
    return do
        let cardSchema = toCardSchema card
            attrSchemas = toAttrSchemas card
            exampleSchemas = toExampleSchemas card
        runInsert cardTable (toRecords cardSchema) trans
        sequence_ do
            schema <- attrSchemas
            return $ runInsert attrTable (toRecords schema) trans
        sequence_ do
            schema <- exampleSchemas
            return $ runInsert exampleTable (toRecords schema) trans

load :: CardID -> IO Card
load _cardid = execCont do
        conn <- contConnection database
        trans <- contTransaction conn
        let val = toSql $ serialize _cardid
        return do
            crs <- runSelect cardTable (columns @CardSchema undefined) ("id = ?", [val]) trans
            css <- mapM (maybeToFail "failed to parse Card table" . fromRecords) crs
            cards <- sequence do
                cs <- css
                return do
                    ars <- runSelect attrTable (columns @AttrSchema undefined) ("cardid = ?", [val]) trans
                    ass <- mapM (maybeToFail "failed to parse Attr table" . fromRecords) ars
                    ers <- runSelect exampleTable (columns @ExampleSchema undefined) ("cardid = ?", [val]) trans
                    ess <- mapM (maybeToFail "failed to parse Example table" . fromRecords) ers
                    maybeToFail "failed to compose Card" $ fromSchemas cs ass ess
            case length cards of
                1 -> return $ head cards
                0 -> fail "Card is not found"
                _ -> fail "multiple Cards are found"

_toHtml card = Tag TD [] [
        Child $ Tag TD [] [Text $ serialize $ language card],
        Child $ Tag TD [] [Text $ word card],
        Child $ Tag TD [] [Text $ meaning card]
    ]

instance Htmlizable Card where
    toHtml = _toHtml
