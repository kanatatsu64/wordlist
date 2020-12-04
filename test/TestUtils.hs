module TestUtils (
    getCardMock,
    getTestUUID
) where

import Serializable ( Serial (..) )
import Card ( Card (..), Language (..), Example (..) )
import UUID ( UUID, fromString )

getCardMock :: String -> IO Card
getCardMock _word = do
    _cardid <- getTestUUID
    _pluginid <- getTestUUID
    let _language = German
        _meaning = "test meaning"
        _attrs = [Serial "test attribute"]
        _note = "test note"
        _examples = [Example "test original" "test translation"]
    return $ Card _cardid _pluginid _language _word _meaning _attrs _note _examples

getTestUUID :: IO UUID
getTestUUID = fromString "00000000-0000-0000-0000-000000000000"
