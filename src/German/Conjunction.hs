module German.Conjunction ( parse, toHtml ) where

import Prelude hiding ( word )

import German.Card ( Card (..) )
import German.Utils ( parse )
import Html ( Html (..), TagName (..), Content (..) )

{-
    Card German Conjunction und [] and _ []

    <tr>
        <td> und </td>
        <td> K. and </td>
        <--! dismiss examples -->
    </tr>
-}

toHtml card = Tag TR [] [Child $ firstTd card, Child $ secondTd card]
    where firstTd card = Tag TD [] [Text $ word card]
          secondTd card = Tag TD [] [Text $ show (part card) ++ " " ++ meaning card ++ _note]
          _note = case note card of
            "" -> ""
            _ -> " (" ++ note card ++ ")"
