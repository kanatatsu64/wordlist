{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Plugins.German.Verb (
    parse,
    
    Kind (..),
    isIntransitive,
    isTransitive,
    parseAttrs
) where

import Serial ( Serial (..) )
import Convertible ( Convertible (..) )
import Plugins.German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )

data Kind = Intransitive | Transitive

instance Convertible Kind String where
    safeConvert Intransitive = Right "I."
    safeConvert Transitive = Right "T."

instance Convertible Kind Serial where
    safeConvert kind = safeConvert kind >>= safeConvert @String

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

parseKindT (_:rests) cons = Just $ \next -> next rests (combine cons Transitive)
    where combine :: ([Serial] -> a) -> Kind -> String -> a
          combine cons kind form = cons [Serial kind, Serial form]
parseKindT _ _ = Nothing

parseKindI (_:_:rests) cons = Just $ \next -> next rests (combine cons Intransitive)
    where combine :: ([Serial] -> a) -> Kind -> a
          combine cons kind = cons [Serial kind]
parseKindI _ _ = Nothing

parseForm (form:rests) cons = Just $ \next -> next rests (cons form)
parseForm _ _ = Nothing
