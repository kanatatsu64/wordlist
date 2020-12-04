module Types (
    CardID,
    Language (..),
    Note,
    Example (..),
    PluginID
) where

import Serializable ( Serializable (..) )
import Composable ( Composable (..) )
import UUID ( UUID )

type CardID = UUID

data Language = Japanese | English | Chinese | French | German

instance Serializable Language where
    serialize Japanese = "Japanese"
    serialize English = "English"
    serialize Chinese = "Chinese"
    serialize French = "French"
    serialize German = "German"

instance Composable Language where
    compose "Japanese" = Just Japanese
    compose "English" = Just English
    compose "Chinese" = Just Chinese
    compose "French" = Just French
    compose "German" = Just German
    compose _ = Nothing

type Note = String

data Example = Example { original :: String, translation :: String }

type PluginID = UUID
