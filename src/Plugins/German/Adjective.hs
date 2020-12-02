module Plugins.German.Adjective (
    parse,

    Comparative (..),
    Superlative (..),
    parseAttrs
) where

import Serializable ( Serializable (..), Serial (..) )
import Plugins.German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )

newtype Comparative = Comparative String

instance Serializable Comparative where
    serialize (Comparative comp) = comp

newtype Superlative = Superlative String

instance Serializable Superlative where
    serialize (Superlative sup) = sup

parse = parseWord >:>
        parseAttrs >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

parseAttrs = parseComparative >:> parseSuperlative

parseComparative (comp:rests) cons = Just $ \next -> next rests (_cons (Comparative comp))
    where _cons comp sup = cons [Serial comp, Serial sup]
parseComparative _ _ = Nothing

parseSuperlative (sup:rests) cons = Just $ \next -> next rests (cons (Superlative sup))
parseSuperlative _ _ = Nothing
