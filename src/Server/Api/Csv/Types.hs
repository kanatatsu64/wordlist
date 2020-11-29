module Server.Api.Csv.Types (
    Language (..),
    Card (..),
    Csv (..),
    convertLanguage,
    convertCard,
    replaceId
) where

import Prelude hiding ( Word )

import qualified Data.UUID as UUID ( toString )
import qualified Data.UUID.V4 as UUID ( nextRandom )

import Server.Json ( Json (..), Ary (..), Dict (..), Rec (..) )
import qualified CardClass ( Language (..), Card (..) )

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

convertLanguage :: CardClass.Language -> Language
convertLanguage CardClass.Japanese = Japanese
convertLanguage CardClass.English = English
convertLanguage CardClass.Chinese =Chinese
convertLanguage CardClass.French = French
convertLanguage CardClass.German = German

convertCard :: CardClass.Card a => a -> Card
convertCard card = Card cardid language word meaning
    where cardid = CardClass.cardid card
          language = convertLanguage $ CardClass.language card
          word = CardClass.word card
          meaning = CardClass.meaning card

data Csv = Csv {
    name :: String,
    cards :: [Card]
}

instance Json Csv where
    jsonify (Csv name cards) = jsonify $ Dict [
            Rec "name" name,
            Rec "cards" (Ary cards)
        ]

replaceId :: Card -> IO Card
replaceId card = do
    uuid <- UUID.nextRandom
    return $ card { cardid = UUID.toString uuid }
