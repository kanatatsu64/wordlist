{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Serializable (
    Serial (..),
    Serializable (..)
) where

import Composable ( Composable (..) )
import UUID ( UUID, toString )

data Serial = forall a. Serializable a => Serial a

instance Composable Serial where
    compose = return . Serial

class Serializable a where
    serialize :: a -> String

instance Serializable String where
    serialize = id

instance Serializable UUID where
    serialize = toString

instance Serializable Serial where
    serialize (Serial s) = serialize s
