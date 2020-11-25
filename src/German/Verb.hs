module German.Verb ( parse, toHtml ) where

import Prelude hiding ( word )

import German.Card ( Card (..), Attr (..) )
import German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )
import Html ( Html (..), TagName (..), Content (..) )

data Kind = Intransitive | Transitive

instance Show Kind where
    show Intransitive = "I."
    show Transitive = "T."

parse = parseWord >:>
        parseAttrs >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

isIntransitive :: String -> Bool
isIntransitive "I" = True
isIntransitive "i" = True
isIntransitive "I." = True
isIntransitive "i." = True
isIntransitive "Intransitive" = True
isIntransitive "intransitive" = True

isIntransitive "Intransitiv" = True
isIntransitive "intransitiv" = True

isIntransitive _ = False

isTransitive :: String -> Bool
isTransitive "T" = True
isTransitive "t" = True
isTransitive "T." = True
isTransitive "t." = True
isTransitive "Transitive" = True
isTransitive "transitive" = True

isTransitive "Transitiv" = True
isTransitive "transitiv" = True

isTransitive _ = False

parseAttrs vals@(kind:_)
    | isIntransitive kind = (parseKindI >:> parseForm) vals
    | isTransitive kind = parseKindT vals

parseKindI (_:rests) cons next = next rests (_cons Intransitive)
    where _cons kind form = cons [Attr kind, Attr form]

parseKindT (_:rests) cons next = next rests (_cons Transitive)
    where _cons kind = cons [Attr kind]

parseForm (form:rests) cons next = next rests (cons form)

{-
    Card German Verb haben [Transitive, - (4)] have illegal [Ich habe eine Haus., I have a hous.]

    <tr>
        <td> haben </td>
        <td> T. have (illegal) </td> <--! dismiss form -->
        <--! dismiss examples -->
    </tr>
-}

toHtml card = Tag TR [] [Child $ firstTd card, Child $ secondTd card]
    where firstTd card = Tag TD [] [Text $ word card]
          secondTd card = Tag TD [] [Text $ show kind ++ " " ++ meaning card ++ _note]
          (kind:_) = attrs card
          _note = case note card of
            "" -> ""
            _ -> " (" ++ note card ++ ")"

