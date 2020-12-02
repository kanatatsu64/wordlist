module Plugins.Base (
    getPluginById
) where

import Plugin ( PluginID, Plugin )
import qualified Plugins.German.Base as German
import Control.Applicative

{-
    === PluginID List ===
    German: c2cc10e1-57d6-4b6f-9899-38d972112d8c
-}

getPluginById :: MonadFail m => PluginID -> m Plugin
getPluginById = when German.uuid German.getPlugin
    where when m a x = do
              y <- m
              if x == y
              then a
              else fail "Plugin is not Found"

infixl 3 <||>

(<||>) :: (Applicative f, Alternative m) => f (m a) -> f (m a) -> f (m a)
(<||>) = liftA2 (<|>)
