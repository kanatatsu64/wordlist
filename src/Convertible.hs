{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Convertible (
    failConvert,
    module Data.Convertible
) where

import Data.Convertible

instance Convertible a a where
    safeConvert = Right

failConvert :: MonadFail m => Convertible a b => a -> m b
failConvert x = case safeConvert x of
    Right val -> return val
    Left error -> fail $ convErrorMessage error
