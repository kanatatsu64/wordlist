{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.List (
    getNameList
) where

import Bundle ( loadInfos )

import Utils ( for )
import Serializable ( Serializable (..) )
import Server.Json ( Json (..), Ary (..), Dict (..), Rec (..) )
import Server.Handler ( handler )
import Server.Response ( json )

getNameList = handler $ do
    infos <- loadInfos
    json $ jsonify $ Ary (for infos $ \(bundleid, name, desc) -> Dict [
            Rec "bundleid" (serialize bundleid),
            Rec "name" name,
            Rec "desc" desc
        ])
