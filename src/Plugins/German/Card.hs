{-# LANGUAGE ExistentialQuantification #-}

module Plugins.German.Card ( 
    Example (..),
    Part (..),
    GermanCard (..),
    convertCard
) where

import Prelude hiding ( Word )

import Control.Monad

import Serializable ( Serializable (..), Serial (..) )
import Card ( Card, CardID, Language ( German ) )
import qualified Card ( Card (..) )
import Utils ( for, (<@>) )

data Example = Example { original :: String, translation :: String }

data Part = Noun | Verb | Adjective | Adverb | Conjunction

instance Serializable Part where
    serialize Noun = "N."
    serialize Verb = "V."
    serialize Adjective = "Adj."
    serialize Adverb = "Adv."
    serialize Conjunction = "Kon."

type Word = String
type Attrs = [Serial]
type Meaning = String
type Note = String
type Examples = [Example]

data GermanCard = GermanCard {
    cardid :: CardID,
    part :: Part,
    word :: Word,
    attrs :: Attrs,
    meaning :: Meaning,
    note :: Note,
    examples :: Examples
}

buildAttributes :: GermanCard -> [Serial]
buildAttributes = do
    _part <- Serial . part
    _attrs <- attrs
    _note <- Serial . note
    _examples <- for <$> examples <@> sequence [Serial . original, Serial . translation]
    return $ [_part] <> _attrs <> [_note] <> join _examples

convertCard :: GermanCard -> Card
convertCard german = Card.Card _cardid _language _word _meaning _attributes
    where
        _cardid = cardid german
        _language = German
        _word = word german
        _meaning = meaning german
        _attributes = buildAttributes german
