module Plugins.German.Verb (
    parse,
    
    Kind (..),
    isIntransitive,
    isTransitive,
    parseAttrs
) where

import Serializable ( Serializable (..), Serial (..) )
import Plugins.German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )

data Kind = Intransitive | Transitive

instance Serializable Kind where
    serialize Intransitive = "I."
    serialize Transitive = "T."

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
    | isIntransitive kind = parseKindI vals
    | isTransitive kind = (parseKindT >:> parseForm) vals

parseKindT (_:rests) cons = Just $ \next -> next rests (_cons Transitive)
    where _cons kind form = cons [Serial kind, Serial form]
parseKindT _ _ = Nothing

parseKindI (_:_:rests) cons = Just $ \next -> next rests (_cons Intransitive)
    where _cons kind = cons [Serial kind]
parseKindI _ _ = Nothing

parseForm (form:rests) cons = Just $ \next -> next rests (cons form)
parseForm _ _ = Nothing
