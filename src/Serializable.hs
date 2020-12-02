{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Serializable (
    Serial (..),
    Serializable (..)
) where

data Serial = forall a. Serializable a => Serial a

class Serializable a where
    serialize :: a -> String

instance Serializable String where
    serialize = id
