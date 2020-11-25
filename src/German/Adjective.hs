module German.Adjective ( parse, toHtml ) where

import Prelude hiding ( word )

import Serializable ( Serializable (..) )
import Html ( Html (..), TagName (..), Content (..) )
import German.Card ( Card (..), Attr (..) )
import German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )

newtype Comparative = Comparative String

instance Serializable Comparative where
    serialize (Comparative comp) = comp

newtype Superlative = Superlative String

instance Serializable Superlative where
    serialize (Superlative sup) = sup

parse = parseWord >:>
        parseAttribute >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

parseAttribute = parseComparative >:> parseSuperlative

parseComparative (comp:rests) cons next = next rests (_cons (Comparative comp))
    where _cons comp sup = cons [Attr comp, Attr sup]

parseSuperlative (sup:rests) cons next = next rests (cons (Superlative sup))

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
          secondTd card = Tag TD [] [Text $ serialize (part card) ++ " " ++ meaning card ++ _note]
          _note = case note card of
            "" -> ""
            _ -> " (" ++ note card ++ ")"

