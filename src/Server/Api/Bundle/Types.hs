{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Api.Bundle.Types (
    AbbrBundle (..)
) where

import Prelude hiding ( lookup )

import Bundle ( BundleID, Bundle (..) )
import Card ( CardID, Card ( cardid ) )
import Convertible ( Convertible (..), ConvertError (..) )
import Server.Json 

data AbbrBundle = AbbrBundle {
    abbr_bundleid :: BundleID,
    abbr_name :: String,
    abbr_desc :: String,
    abbr_cardids :: [CardID]
}

instance Convertible AbbrBundle Bundle where
    safeConvert abbr = Right $ Bundle _bundleid _name _desc []
        where
            _bundleid = abbr_bundleid abbr
            _name = abbr_name abbr
            _desc = abbr_desc abbr

instance Convertible Bundle AbbrBundle where
    safeConvert bundle = Right $ AbbrBundle _bundleid _name _desc _cardids
        where
            _bundleid = bundleid bundle
            _name = name bundle
            _desc = desc bundle
            _cardids = map cardid (cards bundle)

instance Convertible AbbrBundle JObj where
    safeConvert abbr = safeConvert jobject
        where jobject = Dict [
                    Rec "bundleid" (abbr_bundleid abbr),
                    Rec "name" (abbr_name abbr),
                    Rec "desc" (abbr_desc abbr),
                    Rec "cardids" (Ary (abbr_cardids abbr))
                ]

instance Convertible JObj AbbrBundle where
    safeConvert jobj@(JDict jrecs) = do
        bundleid <- lookup "bundleid" jrecs error
        name <- lookup "name" jrecs error
        desc <- lookup "desc" jrecs error
        Ary cardids <- lookup "cardids" jrecs error
        Right $ AbbrBundle bundleid name desc cardids
        where
            sourceValue = show jobj
            sourceType = "JDict"
            destType = "AbbrBundle"
            message = "failed to parse JDict as AbbrBundle"
            error = ConvertError sourceValue sourceType destType message
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "AbbrBundle"
            message = "failed to parse JObj as AbbrBundle"
            error = ConvertError sourceValue sourceType destType message
