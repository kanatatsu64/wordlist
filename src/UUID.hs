module UUID (
    UUID,
    fromString,
    toString,
    getRandom
) where

import Data.UUID ( UUID, toString )
import qualified Data.UUID ( fromString )
import Data.UUID.V4 ( nextRandom )

import Utils ( maybeToFail )

getRandom :: IO UUID
getRandom = nextRandom

fromString :: MonadFail m => String -> m UUID
fromString str = maybeToFail ("invalid UUID: " ++ str) (Data.UUID.fromString str)
