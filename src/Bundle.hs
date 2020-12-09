{-# LANGUAGE FlexibleContexts #-}

module Bundle (
    Bundle (..),
    BundleID
) where

import Prelude hiding ( lookup )

import Html ( Htmlizable (..), TagName (..), Content (..) )
import qualified Html ( Html (..) )
import Card ( Card (..) )
import Types ( BundleID )

data Bundle = Bundle {
    bundleid :: BundleID,
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

_toHtml (Bundle _ name desc cards) = Html.Tag TABLE [] children
    where caption = Html.Tag CAPTION [] [
                  Text $ name ++ _desc
              ]
          _desc = case desc of
                     "" -> ""
                     _ -> " (" ++ desc ++ ")"
          rows = map toHtml cards
          children = map Child (caption:rows)

instance Htmlizable Bundle where
    toHtml = _toHtml
