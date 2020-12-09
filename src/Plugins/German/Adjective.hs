{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Plugins.German.Adjective (
    parse,

    Comparative (..),
    Superlative (..),
    parseAttrs
) where

import Serial ( Serial (..) )
import Convertible ( Convertible (..), convert )
import Plugins.German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )

newtype Comparative = Comparative String

instance Convertible Comparative Serial where
    safeConvert (Comparative comp) = safeConvert comp

newtype Superlative = Superlative String

instance Convertible Superlative Serial where
    safeConvert (Superlative sup) = safeConvert sup

parse = parseWord >:>
        parseAttrs >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

parseAttrs = parseComparative >:> parseSuperlative

parseComparative (comp:rests) cons = Just $ \next -> next rests (combine cons (Comparative comp))
    where combine :: ([Serial] -> a) -> Comparative -> Superlative -> a
          combine cons comp sup = cons [convert comp, convert sup]
parseComparative _ _ = Nothing

parseSuperlative (sup:rests) cons = Just $ \next -> next rests (cons (Superlative sup))
parseSuperlative _ _ = Nothing
