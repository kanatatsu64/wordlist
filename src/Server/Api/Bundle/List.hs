{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.List (
    getNameList
) where

import Utils ( for )
import Server.Bundle ( loadInfos )
import Server.Json ( jsonify, Ary (..), Dict (..), Rec (..) )
import Server.Handler ( handler )
import Server.Response ( json )

getNameList = handler $ do
    infos <- loadInfos
    json $ jsonify $ Ary (for infos $ \(bundleid, name, desc) -> Dict [
            Rec "bundleid" bundleid,
            Rec "name" name,
            Rec "desc" desc
        ])
