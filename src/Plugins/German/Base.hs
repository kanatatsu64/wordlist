module Plugins.German.Base (
    uuid,
    getPlugin,
    GermanCard (..),
    Part (..),
    Genre (..),
    Kind (..) 
) where

import Plugin ()
import Plugins.German.Plugin ( getPlugin, uuid )
import Plugins.German.Card ( GermanCard (..), Part (..) )
import Plugins.German.Noun ( Genre (..) )
import Plugins.German.Verb ( Kind (..) )
