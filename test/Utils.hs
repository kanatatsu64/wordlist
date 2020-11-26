module Utils (
    CardMock (..)
) where

import CardClass ( Card (..) )
import Html ( Htmlizable (..), TagName (..), Html (..), Content (..) )

newtype CardMock = CardMock String

instance Card CardMock where
    word = undefined
    language = undefined
    cardid = undefined
    toCard (desc:_) = CardMock desc
    toCard [] =  CardMock "card mock"

instance Htmlizable CardMock where
    toHtml (CardMock str) = Tag SPAN [] [ Text str ]
