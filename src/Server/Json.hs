{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Server.Json (
    parse,
    jsonify,
    Json (..),
    Ary (..),
    Rec (..),
    Dict (..),

    lookup
) where

import Prelude hiding ( lookup )

import Serial ( Serial )
import UUID ( UUID )
import Convertible ( Convertible (..), ConvertError (..), ConvertResult )
import Card ( Card (..) )
import Bundle ( Bundle (..) )
import Types ( Language (..), Example (..) )
import Server.Internal.Json ( Json (..), JObj (..), JKey, JRec (..), parse, jsonify )

newtype Ary a = Ary [a]
data Rec = forall a. Convertible a JObj => Rec String a
newtype Dict = Dict [Rec]

instance Convertible a JObj => Convertible (Ary a) JObj where
    safeConvert (Ary objs) = JAry <$> mapM safeConvert objs

instance Convertible JObj a => Convertible JObj (Ary a) where
    safeConvert (JAry jary) = Ary <$> mapM safeConvert jary
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "[any]"
            message = "failed to parse JObj as [any]"
            error = ConvertError sourceValue sourceType destType message

instance Convertible Dict JObj where
    safeConvert (Dict recs) = do
        jrecs <- mapM convertRecord recs
        Right . JDict $ jrecs
        where
            convertRecord (Rec key obj) = do
                jobj <- safeConvert obj
                Right $ JRec key jobj

lookup :: Convertible JObj b =>
    JKey -> [JRec] -> ConvertError -> ConvertResult b
lookup key (JRec jkey jobj:jrs) error =
    if key == jkey
    then safeConvert jobj
    else lookup key jrs error
lookup _ [] error = Left error

instance Convertible UUID JObj where
    safeConvert uuid = safeConvert uuid >>= safeConvert @String

instance Convertible JObj UUID where
    safeConvert jobj = safeConvert jobj >>= safeConvert @String

instance Convertible Serial JObj where
    safeConvert serial = safeConvert serial >>= safeConvert @String

instance Convertible JObj Serial where
    safeConvert jobj = safeConvert jobj >>= safeConvert @String

instance Convertible Language JObj where
    safeConvert lang = safeConvert lang >>= safeConvert @String

instance Convertible JObj Language where
    safeConvert jobj = safeConvert jobj >>= safeConvert @String

instance Convertible Example JObj where
    safeConvert (Example original translation) = safeConvert jobject
        where jobject = Dict [
                    Rec "original" original,
                    Rec "translation" translation
                ]

instance Convertible JObj Example where
    safeConvert jobj@(JDict jrecs) = do
        original <- lookup "original" jrecs error
        translation <- lookup "translation" jrecs error
        Right $ Example original translation
        where
            sourceValue = show jobj
            sourceType = "JDict"
            destType = "Example"
            message = "failed to parse JDict as Example"
            error = ConvertError sourceValue sourceType destType message
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "Example"
            message = "failed to parse JObj as Example"
            error = ConvertError sourceValue sourceType destType message

instance Convertible Card JObj where
    safeConvert (Card cardid pluginid language word meaning attrs note examples) = safeConvert jobject
        where jobject = Dict [
                    Rec "cardid" cardid,
                    Rec "pluginid" pluginid,
                    Rec "language" language,
                    Rec "word" word,
                    Rec "meaning" meaning,
                    Rec "attrs" (Ary attrs),
                    Rec "note" note,
                    Rec "examples" (Ary examples)
                ]

instance Convertible JObj Card where
    safeConvert jobj@(JDict jrecs) = do
        cardid <- lookup "cardid" jrecs error
        pluginid <- lookup "pluginid" jrecs error
        language <- lookup "language" jrecs error
        word <- lookup "word" jrecs error
        meaning <- lookup "meaning" jrecs error
        Ary attrs <- lookup "attrs" jrecs error
        note <- lookup "note" jrecs error
        Ary examples <- lookup "examples" jrecs error
        Right $ Card cardid pluginid language word meaning attrs note examples
        where
            sourceValue = show jobj
            sourceType = "JDict"
            destType = "Card"
            message = "failed to parse JDict as Card"
            error = ConvertError sourceValue sourceType destType message
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "Card"
            message = "failed to parse JObj as Card"
            error = ConvertError sourceValue sourceType destType message

instance Convertible Bundle JObj where
    safeConvert (Bundle bundleid name desc cards) = safeConvert jobject
        where jobject = Dict [
                    Rec "bundleid" bundleid,
                    Rec "name" name,
                    Rec "desc" desc,
                    Rec "cards" (Ary cards)
                ]

instance Convertible JObj Bundle where
    safeConvert jobj@(JDict jrecs) = do
        bundleid <- lookup "bundleid" jrecs error
        name <- lookup "name" jrecs error
        desc <- lookup "desc" jrecs error
        Ary cards <- lookup "cards" jrecs error
        Right $ Bundle bundleid name desc cards
        where
            sourceValue = show jobj
            sourceType = "JDict"
            destType = "Bundle"
            message = "failed to parse JDict as Bundle"
            error = ConvertError sourceValue sourceType destType message
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "Bundle"
            message = "failed to parse JObj as Bundle"
            error = ConvertError sourceValue sourceType destType message
