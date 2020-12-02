module Plugins.German.Utils (
    parse,
    parseWord,
    parseAttrs,
    parseMeaning,
    parseNote,
    parseExamples,
    (>:>),

    defaultParse
) where

import Plugins.German.Card ( Example (..) )
import Control.Monad
import Utils

defaultParse :: [a] -> (a -> f) -> Maybe (([a] -> f -> t) -> t)
defaultParse (val:rests) cons = Just $ \next -> next rests (cons val)
defaultParse _ _ = Nothing

parse = parseWord >:>
        parseAttrs >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

parseWord = defaultParse

parseAttrs (_:_:rests) cons = Just $ \next -> next rests (cons [])
parseAttrs _ _ = Nothing

parseMeaning = defaultParse

parseNote = defaultParse

parseExamples vals cons = do
        examples <- loop vals
        return $ cons examples
    where loop (original:translation:rests) = do
              examples <- loop rests
              return $ Example original translation:examples
          loop [] = return []
          loop _ = Nothing

(>:>) :: Monad m => (a -> b -> m (c -> m d)) -> c -> (a -> b -> m d)
f >:> g = \a b -> join $ f a b <@> g
