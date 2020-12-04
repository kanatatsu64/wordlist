{-# LANGUAGE ExistentialQuantification #-}

module Plugins.German.Card ( 
    Example (..),
    Part (..),
    GermanCard (..)
) where

import Prelude hiding ( Word )

import Serializable ( Serializable (..), Serial (..) )
import Types ( CardID, Note ,Example (..) )

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
