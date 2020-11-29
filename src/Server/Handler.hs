{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler (
    Handler,
    handler,
    gets,
    getEnv,
    getBody,
    getParams,
    getRequest,

    sample,
    static,
    file,
    json,
    html,
    htmlFile,
    uploader,
    ok,
    notFound,
    error
) where

import Prelude hiding ( error )

import Control.Monad ( join )
import Network.Wai.Parse ( FileInfo )

import Directory ( (</>), getExt )

import Server.Types ( LazyByteString, ByteString, decode )
import Server.Internal.Handler (
        Handler (..),
        Handlable,
        gets,
        getEnv,
        getBody,
        getParams,
        getRequest
    )
import qualified Server.Response as Response (
        ContentType,
        sample,
        file,
        json,
        html,
        htmlFile,
        uploader,
        ok,
        notFound,
        error
    )

handler :: Handlable h => h -> Handler
handler = Handler

sample :: Handler
sample = handler Response.sample

static :: FilePath -> Handler
static base = handler $ do
    mpath <- lookup "path" <$> getParams
    case join mpath of
        Just (decode -> path) -> return $ Response.file (base </> path) (Just $ ctype path)
        Nothing -> return Response.notFound
    where ctype path = case getExt path of
            "html" -> "text/html"
            "js" -> "text/javascript"
            "csv" -> "text/csv"
            "css" -> "text/css"
            "json" -> "application/json"
            "png" -> "image/png"
            "jpg" -> "image/jpeg"
            "jpeg" -> "image/jpeg"
            _ -> "text/plain"

file :: FilePath -> Maybe Response.ContentType -> Handler
file path ctype = handler $ Response.file path ctype

json :: String -> Handler
json str = handler $ Response.json str

html :: String -> Handler
html str =  handler $ Response.html str

htmlFile :: FilePath -> Handler
htmlFile path = handler $ Response.htmlFile path

uploader :: ((ByteString, FileInfo LazyByteString) -> IO ()) -> Handler
uploader callback = handler $ Response.uploader callback

ok :: Handler
ok = handler Response.ok

notFound :: Handler
notFound = handler Response.notFound

error :: String -> Handler
error msg = handler $ Response.error msg
