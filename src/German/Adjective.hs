module German.Adjective ( parse, toHtml ) where

import Prelude hiding ( word )

import German.Card ( Card (..), Attr (..), Example (..) )
import Html ( Html (..), TagName (..), Content (..) )

data Comparative = Comparative String

instance Show Comparative where
    show (Comparative comp) = comp

data Superlative = Superlative String

instance Show Superlative where
    show (Superlative sup) = sup

parse = parseWord

parseWord (word:rests) cons = parseAttribute rests (cons word)

parseAttribute = parseComparative

parseComparative (comp:rests) cons = parseSuperlative rests (_cons (Comparative comp))
    where _cons comp sup = cons [Attr comp, Attr sup]

parseSuperlative (sup:rests) cons = parseMeaning rests (cons (Superlative sup))

parseMeaning (meaning:rests) cons = parseNote rests (cons meaning)

parseNote (note:rests) cons = parseExamples rests (cons note)

parseExamples vals cons = cons (loop vals)
    where loop (original:translation:rests) = (Example original translation):(loop rests)
          loop [] = []

{-
    Card German Adjective klein [-er, -est] small _ [ein kleines Kind, a small child]

    <tr>
        <td> klein </td>
        <td> Adj. small </td> <--! dismiss comparative & superlative -->
        <--! dismiss examples -->
    </tr>
-}

toHtml card = Tag TR [] [Child $ firstTd card, Child $ secondTd card]
    where firstTd card = Tag TD [] [Text $ word card]
          secondTd card = Tag TD [] [Text $ show (part card) ++ " " ++ meaning card ++ _note]
          _note = case (note card) of
            "" -> ""
            _ -> " (" ++ note card ++ ")"

