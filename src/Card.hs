module Card (
    Card (..),
    Language (..),
    CardID
) where

import Html (Htmlizable (..), Html (..), TagName (..), Content (..) )
import UUID ( UUID )
import Serializable ( Serial )

data Language = Japanese | English | Chinese | French | German
    deriving Show

type CardID = UUID

data Card = Card {
    cardid :: CardID,
    language :: Language,
    word :: String,
    meaning :: String,
    attributes :: [Serial]
}

_toHtml card = Tag TD [] [
        Child $ Tag TD [] [Text $ show $ language card],
        Child $ Tag TD [] [Text $ word card],
        Child $ Tag TD [] [Text $ meaning card]
    ]

instance Htmlizable Card where
    toHtml = _toHtml
