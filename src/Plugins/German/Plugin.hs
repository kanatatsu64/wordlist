module Plugins.German.Plugin ( 
    uuid,
    getPlugin
) where

import UUID ( UUID, fromString )
import Serializable ( Serial (..) )
import qualified Card ( Card (..) )
import Types ( CardID, Language ( German ) )
import Plugin ( Plugin (..) )
import Plugins.German.Card ( GermanCard (..), Part (..) )

import qualified Plugins.German.Noun as Noun ( parse )
import qualified Plugins.German.Verb as Verb( parse )
import qualified Plugins.German.Adjective as Adjective ( parse )
import qualified Plugins.German.Adverb as Adverb ( parse )
import qualified Plugins.German.Conjunction as Conjunction ( parse )

uuid :: MonadFail m => m UUID
uuid = fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

_name :: String
_name = "German"

_desc :: String
_desc = "basic german plugin"


getPlugin :: MonadFail m => m Plugin
getPlugin = do
    _uuid <- uuid
    return $ Plugin _uuid _name _desc _toCard _fromCard

_toCard :: CardID -> [String] -> Maybe Card.Card
_toCard _cardid vals = do
    german <- parsePart vals (GermanCard _cardid)
    convertCard german

_fromCard :: Card.Card -> [String]
_fromCard = undefined

buildAttributes :: GermanCard -> [Serial]
buildAttributes = do
    _part <- Serial . part
    _attrs <- attrs
    return $ [_part] <> _attrs

convertCard :: GermanCard -> Maybe Card.Card
convertCard german = do
    let _cardid = cardid german
        _language = German
        _word = word german
        _meaning = meaning german
        _attributes = buildAttributes german
        _note = note german
        _examples = examples german
    _pluginid <- uuid
    return $ Card.Card _cardid _pluginid _language _word _meaning _attributes _note _examples

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
