module Server.Handler (
    Handler,
    handler,
    gets,
    getEnv,
    getBody,
    getParams,
    getRequest,

    sample,
    html,
    htmlFile,
    uploader,
    ok,
    notFound,
    error
) where

import Prelude hiding ( error )

import Network.Wai.Parse ( FileInfo )

import Server.Types ( LazyByteString, ByteString )
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
        sample,
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

html :: String -> Handler
html str =  handler $ Response.html str

htmlFile :: String -> Handler
htmlFile path = handler $ Response.htmlFile path

uploader :: ((ByteString, FileInfo LazyByteString) -> IO ()) -> Handler
uploader callback = handler $ Response.uploader callback

ok :: Handler
ok = handler Response.ok

notFound :: Handler
notFound = handler Response.notFound

error :: String -> Handler
error msg = handler $ Response.error msg
