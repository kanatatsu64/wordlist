module Server.Api.Types (
    Language (..),
    Card (..),
    Bundle (..),
    convertLanguage,
    convertCard,
    convertBundle
) where

import Prelude hiding ( Word )

import qualified UUID ( toString )

import Serializable ( Serializable (..) )
import Server.Json ( Json (..), Ary (..), Dict (..), Rec (..) )
import qualified Card ( Language (..), Card (..) )
import qualified Bundle ( Bundle (..) )

data Language = Japanese | English | Chinese | French | German

instance Json Language where
    jsonify Japanese = jsonify "Japanese"
    jsonify English = jsonify "English"
    jsonify Chinese = jsonify "Chinese"
    jsonify French = jsonify "French"
    jsonify German = jsonify "German"

data Card = Card {
    cardid :: String,
    language :: Language,
    word :: String,
    meaning :: String,
    attributes :: [String]
}

instance Json Card where
    jsonify (Card cardid language word meaning attributes) = jsonify $ Dict [
            Rec "cardid" cardid,
            Rec "language" language,
            Rec "word" word,
            Rec "meaning" meaning,
            Rec "attributes" (Ary attributes)
        ]

convertLanguage :: Card.Language -> Language
convertLanguage Card.Japanese = Japanese
convertLanguage Card.English = English
convertLanguage Card.Chinese =Chinese
convertLanguage Card.French = French
convertLanguage Card.German = German

convertCard :: Card.Card -> Card
convertCard card = Card cardid language word meaning attributes
    where cardid = UUID.toString $ Card.cardid card
          language = convertLanguage $ Card.language card
          word = Card.word card
          meaning = Card.meaning card
          attributes = map serialize $ Card.attributes card

data Bundle = Bundle {
    name :: String,
    desc :: String,
    cards :: [Card]
}

instance Json Bundle where
    jsonify (Bundle name desc cards) = jsonify $ Dict [
            Rec "name" name,
            Rec "desc" desc,
            Rec "cards" (Ary cards)
        ]

convertBundle :: Bundle.Bundle -> Bundle
convertBundle bundle = Bundle name desc cards
    where name = Bundle.name bundle
          desc = Bundle.desc bundle
          cards = map convertCard $ Bundle.cards bundle
