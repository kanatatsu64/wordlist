module German.Adverb ( parse, toHtml ) where

import Prelude hiding ( word )

import German.Card ( Card (..), Example (..) )
import Html ( Html (..), TagName (..), Content (..) )

parse = parseWord

parseWord (word:rests) cons = parseAttribute rests (cons word)

parseAttribute (_:_:rests) cons = parseMeaning rests (cons [])

parseMeaning (meaning:rests) cons = parseNote rests (cons meaning)

parseNote (note:rests) cons = parseExamples rests (cons note)

parseExamples vals cons = cons (loop vals)
    where loop (original:translation:rests) = (Example original translation):(loop rests)
          loop [] = []

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
          secondTd card = Tag TD [] [Text $ show (part card) ++ " " ++ meaning card ++ _note]
          _note = case (note card) of
            "" -> ""
            _ -> " (" ++ note card ++ ")"

