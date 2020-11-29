module Server.Api.Csv.Types (
    Language (..),
    Word (..),
    Csv (..),
    convertLanguage,
    convertCardToWord
) where

import Prelude hiding ( Word )

import Server.Json ( Json (..), Ary (..), Dict (..), Rec (..) )
import qualified CardClass ( Language (..), Card (..) )

data Language = Japanese | English | Chinese | French | German

instance Json Language where
    jsonify Japanese = jsonify "japanese"
    jsonify English = jsonify "english"
    jsonify Chinese = jsonify "chinese"
    jsonify French = jsonify "french"
    jsonify German = jsonify "german"

data Word = Word {
    language :: Language,
    word :: String,
    meaning :: String
}

instance Json Word where
    jsonify (Word language word meaning) = jsonify $ Dict [
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

convertCardToWord :: CardClass.Card a => a -> Word
convertCardToWord card = Word language word meaning
    where language = convertLanguage $ CardClass.language card
          word = CardClass.word card
          meaning = CardClass.meaning card

data Csv = Csv {
    name :: String,
    words :: [Word]
}

instance Json Csv where
    jsonify (Csv name words) = jsonify $ Dict [
            Rec "name" name,
            Rec "words" (Ary words)
        ]
