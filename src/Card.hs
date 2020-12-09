{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Card (
    Card (..),
    CardID,
    Language (..),
    Note,
    Example (..),
) where

import Prelude hiding ( lookup )

import Html (Htmlizable (..), Html (..), TagName (..), Content (..) )
import Serial ( Serial )
import Convertible ( convert )
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

_toHtml card = Tag TD [] [
        Child $ Tag TD [] [Text $ convert $ language card],
        Child $ Tag TD [] [Text $ word card],
        Child $ Tag TD [] [Text $ meaning card]
    ]

instance Htmlizable Card where
    toHtml = _toHtml
