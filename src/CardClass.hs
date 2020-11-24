module CardClass ( Card (..), Language (..), CardID ) where

import Html ( Html )

data Language = Japanese | Chinese | French | German

type CardID = String

class Card a where
    language :: a -> Language
    cardid :: a -> CardID
    word :: a -> String
    toHtml :: a -> Html
    toCard :: [String] -> a
 
