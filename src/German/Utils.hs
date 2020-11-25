module German.Utils (
    parse,
    parseWord,
    parseAttrs,
    parseMeaning,
    parseNote,
    parseExamples,
    (>:>),

    defaultParse
) where

import German.Card ( Example (..) )

defaultParse :: [a] -> (a -> f) -> ([a] -> f -> t) -> t
defaultParse (val:rests) cons next = next rests (cons val)

parse = parseWord >:>
        parseAttrs >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

parseWord = defaultParse

parseAttrs (_:_:rests) cons next = next rests (cons [])

parseMeaning = defaultParse

parseNote = defaultParse

parseExamples vals cons = cons (loop vals)
    where loop (original:translation:rests) = Example original translation:loop rests
          loop [] = []

(>:>) :: (a -> b -> (c -> d -> e) -> e) -> (c -> d -> e) -> (a -> b -> e)
f >:> g = \a b -> f a b g
