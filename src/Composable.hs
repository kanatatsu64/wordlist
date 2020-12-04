{-# LANGUAGE FlexibleInstances #-}

module Composable (
    Composable (..)
) where

import UUID ( UUID, fromString )

class Composable a where
    compose :: String -> Maybe a

instance Composable String where
    compose = return

instance Composable UUID where
    compose = fromString
