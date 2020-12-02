{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Serializable (
    Serial (..),
    Serializable (..)
) where

import UUID ( UUID, toString )

data Serial = forall a. Serializable a => Serial a

class Serializable a where
    serialize :: a -> String

instance Serializable String where
    serialize = id

instance Serializable UUID where
    serialize = toString

instance Serializable Serial where
    serialize (Serial s) = serialize s
