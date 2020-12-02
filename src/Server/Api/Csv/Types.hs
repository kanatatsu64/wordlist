module Server.Api.Csv.Types (
    Language (..),
    Card (..),
    Csv (..),
    convertLanguage,
    convertCard
) where

import Prelude hiding ( Word )

import qualified UUID ( toString )

import Server.Json ( Json (..), Ary (..), Dict (..), Rec (..) )
import qualified Card ( Language (..), Card (..) )

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
    meaning :: String
}

instance Json Card where
    jsonify (Card cardid language word meaning) = jsonify $ Dict [
            Rec "cardid" cardid,
            Rec "language" language,
            Rec "word" word,
            Rec "meaning" meaning
        ]

convertLanguage :: Card.Language -> Language
convertLanguage Card.Japanese = Japanese
convertLanguage Card.English = English
convertLanguage Card.Chinese =Chinese
convertLanguage Card.French = French
convertLanguage Card.German = German

convertCard :: Card.Card -> Card
convertCard card = Card cardid language word meaning
    where cardid = UUID.toString $ Card.cardid card
          language = convertLanguage $ Card.language card
          word = Card.word card
          meaning = Card.meaning card

data Csv = Csv {
    name :: String,
    cards :: [Card]
}

instance Json Csv where
    jsonify (Csv name cards) = jsonify $ Dict [
            Rec "name" name,
            Rec "cards" (Ary cards)
        ]
