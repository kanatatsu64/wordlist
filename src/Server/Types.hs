module Server.Types (
    ByteString,
    LazyByteString,
    lazyEncode,
    encode,
    lazyDecode,
    decode,
    insert,
    upsert,
    cons,
    exists,
    lookup,
    Path,
    Body,
    Param
) where

import Prelude hiding ( lookup )

import Data.Text ( Text )
import qualified Data.Text ( pack, unpack )
import qualified Data.Text.Lazy ( pack, unpack )
import qualified Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import qualified Data.Text.Lazy.Encoding ( encodeUtf8, decodeUtf8 )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy ( ByteString )

import Utils ( maybeToFail )

type LazyByteString = Data.ByteString.Lazy.ByteString

lazyEncode :: String -> LazyByteString
lazyEncode = Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.pack

encode :: String -> ByteString
encode = Data.Text.Encoding.encodeUtf8 . Data.Text.pack

lazyDecode :: LazyByteString -> String
lazyDecode = Data.Text.Lazy.unpack . Data.Text.Lazy.Encoding.decodeUtf8

decode :: ByteString -> String
decode = Data.Text.unpack . Data.Text.Encoding.decodeUtf8

type Path = [Text]
type Body = LazyByteString
type Key = ByteString
type Value = ByteString
type Param = (Key, Maybe Value)

insert :: Param -> [Param] -> [Param]
insert p@(key, _) ps
    | exists key ps = ps
    | otherwise = p:ps

upsert :: Param -> [Param] -> [Param]
upsert p@(key, _) (p'@(key', _):ps)
    | key == key' = p:ps
    | otherwise = p':upsert p ps
upsert p [] = [p]

cons :: [Param] -> [Param] -> [Param]
cons base (a:as) = cons (insert a base) as
cons base [] = base

exists :: Key -> [Param] -> Bool
exists key ((key', _):ps)
    | key == key' = True
    | otherwise = exists key ps
exists _ [] = False

lookup :: MonadFail m => Key -> [Param] -> m Value
lookup key ((key', mval'):ps)
    | key == key' = maybeToFail "parameter does not have a value" mval'
    | otherwise = lookup key ps
lookup key [] = fail $ "parameter is not found: " ++ decode key
