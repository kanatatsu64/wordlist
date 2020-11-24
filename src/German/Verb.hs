module German.Verb ( parse, toHtml ) where

import Prelude hiding ( word )

import German.Card ( Card (..), Attr (..), Example (..) )
import Html ( Html (..), TagName (..), Content (..) )

data Kind = Intransitive | Transitive

instance Show Kind where
    show Intransitive = "I."
    show Transitive = "T."

parse = parseWord

parseWord (word:rests) cons = parseAttrs rests (cons word)

parseAttrs = parseKind

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

parseKind (kind:rests) cons | isIntransitive kind = parseForm rests (_cons Intransitive)
    where _cons kind form = cons [Attr kind, Attr form]
parseKind (kind:_:rests) cons | isTransitive kind = parseMeaning rests (_cons Transitive)
    where _cons kind = cons [Attr kind]

parseForm (form:rests) cons = parseMeaning rests (cons form)

parseMeaning (meaning:rests) cons = parseNote rests (cons meaning)

parseNote (note:rests) cons = parseExamples rests (cons note)

parseExamples vals cons = cons (loop vals)
    where loop (original:translation:rests) = (Example original translation):(loop rests)
          loop [] = []

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
          _note = case (note card) of
            "" -> ""
            _ -> " (" ++ note card ++ ")"

