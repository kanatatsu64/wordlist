{-# LANGUAGE FlexibleInstances #-}

module Serializable (
    Serializable (..)
) where

class Serializable a where
    serialize :: a -> String

instance Serializable String where
    serialize = id
