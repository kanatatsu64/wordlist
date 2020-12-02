{-# LANGUAGE ExistentialQuantification #-}

module Bundle ( Bundle (..) ) where

import Html ( Htmlizable (..), TagName (..), Html (..), Content (..) )
import Card ( Card )

data Bundle = Bundle {
    name :: String,
    desc :: String,
    cards :: [Card]
}

{-
    Bundle "german" "from textbook" [..]

    <table>
        <caption> german (from textbook) </caption>
        <tr> .. </tr>
        ...
    </table>
-}

_toHtml (Bundle name desc cards) = Tag TABLE [] children
    where caption = Tag CAPTION [] [
                  Text $ name ++ _desc
              ]
          _desc = case desc of
                     "" -> ""
                     _ -> " (" ++ desc ++ ")"
          rows = map toHtml cards
          children = map Child (caption:rows)

instance Htmlizable Bundle where
    toHtml = _toHtml
