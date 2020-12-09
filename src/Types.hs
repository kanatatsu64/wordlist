{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types (
    CardID,
    Language (..),
    Note,
    Example (..),
    PluginID,
    BundleID
) where

import Data.Convertible
import Serial ( Serial )
import UUID ( UUID )

type CardID = UUID

data Language = Japanese | English | Chinese | French | German

instance Convertible Language String where
    safeConvert Japanese = Right "Japanese"
    safeConvert English = Right "English"
    safeConvert Chinese = Right "Chinese"
    safeConvert French = Right "French"
    safeConvert German = Right "German"

instance Convertible String Language where
    safeConvert "Japanese" = Right Japanese
    safeConvert "English" = Right English
    safeConvert "Chinese" = Right Chinese
    safeConvert "French" = Right French
    safeConvert "German" = Right German
    safeConvert str = Left error
        where
            sourceValue = str
            sourceType = "String"
            destType = "Language"
            message = "failed to parse string"
            error = ConvertError sourceValue sourceType destType message

instance Convertible Language Serial where
    safeConvert lang = safeConvert lang >>= safeConvert @String

instance Convertible Serial Language where
    safeConvert serial = safeConvert serial >>= safeConvert @String

type Note = String

data Example = Example { original :: String, translation :: String }

type PluginID = UUID

type BundleID = UUID
