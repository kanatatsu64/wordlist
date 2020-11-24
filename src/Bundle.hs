{-# LANGUAGE ExistentialQuantification #-}

module Bundle ( Bundle (..) ) where

import CardClass ( Card )

data Bundle = forall c. Card c => Bundle {
    name :: String,
    desc :: String,
    cards :: [c]
}

