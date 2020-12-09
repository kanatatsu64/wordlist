{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Serial (
    Serial (..),
    serialize,
    safeSerialize
) where

import Convertible ( Convertible (..), ConvertResult, convert )

data Serial = forall s. Convertible s String => Serial s

safeSerialize :: Serial -> ConvertResult String
safeSerialize = safeConvert

serialize :: Serial -> String
serialize = convert

instance Convertible Serial String where
    safeConvert (Serial s) = safeConvert s

instance Convertible String Serial where
    safeConvert = Right . Serial
