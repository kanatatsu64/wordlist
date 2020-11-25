module German.Adverb ( parse, toHtml ) where

import Prelude hiding ( word )

import Serializable ( Serializable (..) )
import Html ( Html (..), TagName (..), Content (..) )
import German.Card ( Card (..) )
import German.Utils ( parse )

{-
    Card German Adverb schon [] already _ []

    <tr>
        <td> schon </td>
        <td> Adv. already </td>
        <--! dismiss examples -->
    </tr>
-}

toHtml card = Tag TR [] [Child $ firstTd card, Child $ secondTd card]
    where firstTd card = Tag TD [] [Text $ word card]
          secondTd card = Tag TD [] [Text $ serialize (part card) ++ " " ++ meaning card ++ _note]
          _note = case note card of
            "" -> ""
            _ -> " (" ++ note card ++ ")"
