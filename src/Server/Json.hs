{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Server.Json (
    Json (..),
    Ary (..),
    Dict (..),
    Rec (..)
) where

newtype Ary a = Ary [a]
data Rec = forall a. Json a => Rec String a
newtype Dict = Dict [Rec]

class Json a where
    jsonify :: a -> String

instance Json String where
    jsonify str = "\"" ++ str ++ "\""

instance Json Int where
    jsonify = jsonify . show

instance Json Float where
    jsonify = jsonify . show

instance Json a => Json (Ary a) where
    jsonify (Ary vs) = "[" ++ loop vs ++ "]"
        where loop [] = ""
              loop [x] = jsonify x
              loop (x:vs) = jsonify x ++ "," ++ loop vs

instance Json Dict where
    jsonify (Dict rs) = "{" ++ loop rs ++ "}"
        where loop [] = ""
              loop [r] = jsonifyRecord r
              loop (r:rs) = jsonifyRecord r ++ "," ++ loop rs
              jsonifyRecord (Rec key val) = jsonify key ++ ":" ++ jsonify val
