module CardClass (
    Card (..),
    Language (..),
    CardID
) where

data Language = Japanese | Chinese | French | German

type CardID = String

class Card a where
    language :: a -> Language
    cardid :: a -> CardID
    word :: a -> String
    toCard :: [String] -> a
