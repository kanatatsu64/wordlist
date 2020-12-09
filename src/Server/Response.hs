{-# LANGUAGE OverloadedStrings #-}

module Server.Response (
    ContentType,
    sample,
    html,
    json,
    file,
    htmlFile,
    uploader,
    notFound,
    ok,
    error
) where

import Prelude hiding ( error )

import Network.HTTP.Types.Status ( status200, status404, status500 )
import Network.Wai ( responseFile, responseLBS, Request, Response )
import Network.HTTP.Types.Header ( hContentType )
import Network.Wai.Parse ( FileInfo )

import Server.Json ( Json (..) )
import Server.Types ( lazyEncode, LazyByteString, ByteString )
import Server.Utils ( upload )

type ContentType = ByteString

sample :: IO Response
sample = return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

file :: FilePath -> Maybe ContentType-> IO Response
file path (Just ctype) = return $ responseFile status200 [(hContentType, ctype)] path Nothing
file path Nothing = return $ responseFile status200 [] path Nothing

html :: String -> IO Response
html str = return $ responseLBS status200 [(hContentType, "text/html")] $ lazyEncode str

htmlFile :: FilePath -> IO Response
htmlFile path = return $ responseFile status200 [(hContentType, "text/html")] path Nothing

json :: Json -> IO Response
json (Json str) = return $ responseLBS status200 [(hContentType, "application/json")] $ lazyEncode str

uploader :: ((ByteString, FileInfo LazyByteString) -> IO ()) -> Request -> IO Response
uploader callback request = do
    upload callback request
    ok

ok :: IO Response
ok = return $ responseLBS status200 [] ""

notFound :: IO Response
notFound = return $ responseLBS status404 [] ""

error :: String -> IO Response
error msg = return $ responseLBS status500 [] (lazyEncode msg)
