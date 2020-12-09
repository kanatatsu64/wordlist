{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
    Param,
    queryToParams,
    buildPath,
    isRoot,
    isAbsolute,
    isRelative,
    toAbsolute,
    isDirectory,
    isFile,
    (</>)
) where

import Prelude hiding ( lookup )

import Network.HTTP.Types ( Query )
import Data.Text ( Text )
import qualified Data.Text ( pack, unpack )
import qualified Data.Text.Lazy ( pack, unpack )
import qualified Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import qualified Data.Text.Lazy.Encoding ( encodeUtf8, decodeUtf8 )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy ( ByteString )

import UUID ( UUID )
import Convertible ( Convertible (..) )
import Utils ( maybeToFail, split, trimLast )

type LazyByteString = Data.ByteString.Lazy.ByteString

lazyEncode :: String -> LazyByteString
lazyEncode = Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.pack

encode :: String -> ByteString
encode = Data.Text.Encoding.encodeUtf8 . Data.Text.pack

lazyDecode :: LazyByteString -> String
lazyDecode = Data.Text.Lazy.unpack . Data.Text.Lazy.Encoding.decodeUtf8

decode :: ByteString -> String
decode = Data.Text.unpack . Data.Text.Encoding.decodeUtf8

instance Convertible String LazyByteString where
    safeConvert = Right . lazyEncode

instance Convertible LazyByteString String where
    safeConvert = Right . lazyDecode

instance Convertible String ByteString where
    safeConvert = Right . encode

instance Convertible ByteString String where
    safeConvert = Right . decode

instance Convertible UUID LazyByteString where
    safeConvert uuid = safeConvert uuid >>= safeConvert @String

instance Convertible LazyByteString UUID where
    safeConvert lbstr = safeConvert lbstr >>= safeConvert @String

instance Convertible UUID ByteString where
    safeConvert uuid = safeConvert uuid >>= safeConvert @String

instance Convertible ByteString UUID where
    safeConvert bstr = safeConvert bstr >>= safeConvert @String

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

queryToParams :: Query -> [Param]
queryToParams = id

buildPath :: String -> IO Path
buildPath raw = map Data.Text.pack <$> (validate . shrink) parts
    where parts = split '/' raw
          validate [] = return []
          validate ps@[_] = return ps
          validate (p:ps) = case p of
              '*':_ -> fail $ "invalid pattern: " ++ raw
              _ -> (p:) <$> validate ps

{-
    (shrink . split '/') "/" = [] (Root, Directory)
    (shrink . split '/') "/dir//path" = ["", "dir", "path"] (Absolute, File)
    (shrink . split '/') "/dir//path/" = ["", "dir", "path", ""] (Absolute, Directory)
    (shrink . split '/') "dir//path" = ["dir", "path"] (Relative, File)
    (shrink . split '/') "dir/path/" = ["dir", "path", ""] (Relative, Directory)
-}

shrink :: [String] -> [String]
shrink [""] = [""]
shrink ["", ""] = []
shrink ("":path) = "":_shrink path
shrink path = _shrink path

_shrink :: [String] -> [String]
_shrink [] = []
_shrink [""] = [""]
_shrink ("":path) = path
_shrink (p:ps) = p:_shrink ps

isRoot :: Path -> Bool
isRoot [] = True
isRoot _ = False

isAbsolute :: Path -> Bool
isAbsolute [] = True
isAbsolute [""] = False
isAbsolute ("":_) = True 
isAbsolute _ = False

isRelative :: Path -> Bool
isRelative = not . isAbsolute

toAbsolute :: Path -> Path
toAbsolute [] = []
toAbsolute path@("":_) = path
toAbsolute path = "":path

isDirectory :: Path -> Bool
isDirectory [] = True
isDirectory [""] = False
isDirectory (last -> "") = True
isDirectory _ = False

isFile :: Path -> Bool
isFile = not . isDirectory

(</>) :: Path -> Path -> Path
base </> path
    | isAbsolute path = path
    | isRoot base = toAbsolute path
    | otherwise = trimDir base <> path
    where trimDir path@(last -> "") = trimLast path
          trimDir path = path
