module German.Proc ( 
    toCard,
    toHtml
) where

import qualified CardClass ( Card (..) )
import CardClass ( Language ( German ) )

import German.Card ( Card (..), Part (..) )
import Html ( Html (..) )

import qualified German.Noun as Noun ( parse, toHtml )
import qualified German.Verb as Verb( parse, toHtml )
import qualified German.Adjective as Adjective ( parse, toHtml )
import qualified German.Adverb as Adverb ( parse, toHtml )
import qualified German.Conjunction as Conjunction ( parse, toHtml )

toCard :: [String] -> Card
toCard vals = parsePart vals (Card cardid)
    where cardid = "not implemented"

isNoun :: String -> Bool
isNoun "N" = True
isNoun "n" = True
isNoun "N." = True
isNoun "n." = True
isNoun "Noun" = True
isNoun "noun" = True

isNoun "Nomen" = True
isNoun "nomen" = True

isNoun _ = False

isVerb :: String -> Bool
isVerb "V" = True
isVerb "v" = True
isVerb "V." = True
isVerb "v." = True
isVerb "Verb" = True
isVerb "verb" = True

isVerb _ = False

isAdjective :: String -> Bool
isAdjective "Adj" = True
isAdjective "adj" = True
isAdjective "Adj." = True
isAdjective "adj." = True
isAdjective "Adjective" = True
isAdjective "adjective" = True

isAdjective "Adjektiv" = True
isAdjective "adjektiv" = True

isAdjective _ = False

isAdverb :: String -> Bool
isAdverb "Adv" = True
isAdverb "adv" = True
isAdverb "Adv." = True
isAdverb "adv." = True
isAdverb "Adverb" = True
isAdverb "adverb" = True

isAdverb _ = False

isConjunction :: String -> Bool
isConjunction "C" = True
isConjunction "c" = True
isConjunction "C." = True
isConjunction "c." = True
isConjunction "Con" = True
isConjunction "con" = True
isConjunction "Con." = True
isConjunction "con." = True
isConjunction "Conjunction" = True
isConjunction "conjunction" = True

isConjunction "K" = True
isConjunction "k" = True
isConjunction "K." = True
isConjunction "k." = True
isConjunction "Kon" = True
isConjunction "kon" = True
isConjunction "Kon." = True
isConjunction "kon." = True
isConjunction "Konjunction" = True
isConjunction "konjunction" = True

isConjunction _ = False

parsePart (part:rests) cons 
    | isNoun part = Noun.parse rests (cons Noun)
    | isVerb part = Verb.parse rests (cons Verb)
    | isAdjective part = Adjective.parse rests (cons Adjective)
    | isAdverb part = Adverb.parse rests (cons Adverb)
    | isConjunction part = Conjunction.parse rests (cons Conjunction)

toHtml :: Card -> Html
toHtml card = case (part card) of
    Noun -> Noun.toHtml card
    Verb -> Verb.toHtml card
    Adjective -> Adjective.toHtml card
    Adverb -> Adverb.toHtml card
    Conjunction -> Conjunction.toHtml card

instance CardClass.Card Card where
    cardid = cardid
    language = const German
    word = word
    toHtml = toHtml
    toCard = toCard

