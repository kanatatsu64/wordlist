{-# LANGUAGE ExistentialQuantification #-}

module German.Card ( 
    Example (..),
    Attr (..),
    Part (..),
    Card (..)
) where

import Prelude hiding ( Word )

import Serializable ( Serializable (..) )
import CardClass ( CardID )

data Example = Example { original :: String, translation :: String }
data Attr = forall a. Serializable a => Attr a

instance Serializable Attr where
    serialize (Attr attr) = serialize attr

data Part = Noun | Verb | Adjective | Adverb | Conjunction

instance Serializable Part where
    serialize Noun = "N."
    serialize Verb = "V."
    serialize Adjective = "Adj."
    serialize Adverb = "Adv."
    serialize Conjunction = "Kon."

type Word = String
type Attrs = [Attr]
type Meaning = String
type Note = String
type Examples = [Example]

data Card = Card {
    cardid :: CardID,
    part :: Part,
    word :: Word,
    attrs :: Attrs,
    meaning :: Meaning,
    note :: Note,
    examples :: Examples
}
