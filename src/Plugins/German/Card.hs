{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Plugins.German.Card ( 
    Example (..),
    Part (..),
    GermanCard (..)
) where

import Prelude hiding ( Word )

import Serial ( Serial (..) )
import Convertible ( Convertible (..) )
import Types ( CardID, Note ,Example (..) )

data Part = Noun | Verb | Adjective | Adverb | Conjunction

instance Convertible Part String where
    safeConvert Noun = Right "N."
    safeConvert Verb = Right "V."
    safeConvert Adjective = Right "Adj."
    safeConvert Adverb = Right "Adv."
    safeConvert Conjunction = Right "Kon."

instance Convertible Part Serial where
    safeConvert part = safeConvert part >>= safeConvert @String

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
