{-# LANGUAGE ExistentialQuantification #-}

module German.Card ( 
    Example (..),
    Attr (..),
    Part (..),
    Card (..)
) where

import Prelude hiding ( Word )

import CardClass ( CardID )

data Example = Example { original :: String, translation :: String }
data Attr = forall a. Show a => Attr a

instance Show Attr where
    show (Attr attr) = show attr

data Part = Noun | Verb | Adjective | Adverb | Conjunction

instance Show Part where
    show Noun = "N."
    show Verb = "V."
    show Adjective = "Adj."
    show Adverb = "Adv."
    show Conjunction = "Kon."

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

