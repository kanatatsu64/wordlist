module TestUtils (
    getCardMock,
    getTestUUID
) where

import Serializable ( Serial (..) )
import Card ( Card (..), Language (..) )
import UUID ( UUID, fromString )

getCardMock :: String -> IO Card
getCardMock _word = do
    _cardid <- getTestUUID
    let _language = German
        _meaning = "test meaning"
        _attributes = [Serial "test attribute"]
    return $ Card _cardid _language _word _meaning _attributes

getTestUUID :: IO UUID
getTestUUID = fromString "00000000-0000-0000-0000-000000000000"
