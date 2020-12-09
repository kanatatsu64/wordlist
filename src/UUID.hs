{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module UUID (
    UUID,
    fromString,
    toString,
    getRandom
) where

import Data.UUID ( UUID, toString )
import qualified Data.UUID ( fromString )
import Data.UUID.V4 ( nextRandom )

import Convertible ( Convertible (..), ConvertError (..) )
import Utils ( maybeToFail )

instance Convertible String UUID where
    safeConvert str = case fromString str of
        Right uuid -> Right uuid
        Left message -> Left $ getError message
        where
            sourceValue = str
            sourceType = "String"
            destType = "UUID"
            getError = ConvertError sourceValue sourceType destType

instance Convertible UUID String where
    safeConvert = Right . toString

getRandom :: IO UUID
getRandom = nextRandom

fromString :: MonadFail m => String -> m UUID
fromString str = maybeToFail ("invalid UUID: " ++ str) (Data.UUID.fromString str)
