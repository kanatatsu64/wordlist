module German.Noun ( parse, toHtml ) where

import Prelude hiding ( word )

import German.Card ( Card (..), Attr (..), Example (..) )
import Html ( Html (..), TagName (..), Content (..) )

data Genre = Male | Female | Neuter

instance Show Genre where
    show Male = "M."
    show Female = "F."
    show Neuter = "N."

parse = parseWord

parseWord (word:rests) cons = parseAttrs rests (cons word)

isMale :: String -> Bool
isMale "M." = True
isMale "m." = True
isMale "Male" = True
isMale "male" = True

isMale "Maskulinum" = True
isMale "maskulinum" = True

isMale _ = False

isFemale :: String -> Bool
isFemale "F." = True
isFemale "f." = True
isFemale "Female" = True
isFemale "female" = True

isFemale "Femininum" = True
isFemale "femininum" = True

isFemale _ = False

isNeuter :: String -> Bool
isNeuter "N." = True
isNeuter "n." = True
isNeuter "Neuter" = True
isNeuter "neuter" = True

isNeuter "Neutrum" = True
isNeuter "neutrum" = True

isNeuter _ = False

parseAttrs (pl:gen:rests) cons
    | isMale gen = parseMeaning rests (cons [Attr pl, Attr Male])
    | isFemale gen = parseMeaning rests (cons [Attr pl, Attr Female])
    | isNeuter gen = parseMeaning rests (cons [Attr pl, Attr Neuter])

parseMeaning (meaning:rests) cons = parseNote rests (cons meaning)

parseNote (note:rests) cons = parseExamples rests (cons note)

parseExamples vals cons = cons (loop vals)
    where loop (original:translation:rests) = (Example original translation):(loop rests)
          loop [] = []

{-
    Card German Noun Name [-en, Male] name illegal []

    <tr>
        <td> Name - Namen </td>
        <td> M. name (illegal) </td>
        <--! dismiss examples -->
    </tr>
-}

toHtml card = Tag TR [] [Child $ firstTd card, Child $ secondTd card]
    where firstTd card = Tag TD [] [Text $ word card ++ " - " ++ consPl (show pl) (word card)] 
          secondTd card = Tag TD [] [Text $ show gen ++ " " ++ meaning card ++ _note]
          (pl:gen:[]) = attrs card
          _note = case (note card) of
            "" -> ""
            _ -> " (" ++ note card ++ ")"

consPl :: String -> String -> String
consPl ('-':tail) word = word ++ tail
consPl pl _ = pl

