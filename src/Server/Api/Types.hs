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
import qualified Card ( Card (..) )
import qualified Bundle ( Bundle (..) )
import qualified Types ( Language (..) )

data Language = Japanese | English | Chinese | French | German

instance Json Language where
    jsonify Japanese = jsonify "Japanese"
    jsonify English = jsonify "English"
    jsonify Chinese = jsonify "Chinese"
    jsonify French = jsonify "French"
    jsonify German = jsonify "German"

data Card = Card {
    cardid :: String,
    pluginid :: String,
    language :: Language,
    word :: String,
    meaning :: String,
    attributes :: [String]
}

instance Json Card where
    jsonify (Card cardid pluginid language word meaning attrs) = jsonify $ Dict [
            Rec "cardid" cardid,
            Rec "pluginid" pluginid,
            Rec "language" language,
            Rec "word" word,
            Rec "meaning" meaning,
            Rec "attrs" (Ary attrs)
        ]

convertLanguage :: Types.Language -> Language
convertLanguage Types.Japanese = Japanese
convertLanguage Types.English = English
convertLanguage Types.Chinese =Chinese
convertLanguage Types.French = French
convertLanguage Types.German = German

convertCard :: Card.Card -> Card
convertCard card = Card cardid pluginid language word meaning attrs
    where cardid = UUID.toString $ Card.cardid card
          pluginid = UUID.toString $ Card.pluginid card
          language = convertLanguage $ Card.language card
          word = Card.word card
          meaning = Card.meaning card
          attrs = map serialize $ Card.attrs card

data Bundle = Bundle {
    bundleid :: String,
    name :: String,
    desc :: String,
    cards :: [Card]
}

instance Json Bundle where
    jsonify (Bundle bundleid name desc cards) = jsonify $ Dict [
            Rec "bundleid" bundleid,
            Rec "name" name,
            Rec "desc" desc,
            Rec "cards" (Ary cards)
        ]

convertBundle :: Bundle.Bundle -> Bundle
convertBundle bundle = Bundle bundleid name desc cards
    where bundleid = UUID.toString $ Bundle.bundleid bundle
          name = Bundle.name bundle
          desc = Bundle.desc bundle
          cards = map convertCard $ Bundle.cards bundle
