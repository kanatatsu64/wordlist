module Server.Utils (
    upload,
    body
) where

import Network.Wai ( Request, lazyRequestBody )
import Network.Wai.Parse ( parseRequestBody, lbsBackEnd, FileInfo )

import Server.Types ( ByteString, LazyByteString )

upload :: ((ByteString, FileInfo LazyByteString) -> IO a) -> Request -> IO [a]
upload callback request = do
    (_, files) <- parseRequestBody lbsBackEnd request
    mapM callback files

body :: (LazyByteString -> IO a) -> Request -> IO a
body callback request = do
    bstr <- lazyRequestBody request
    callback bstr
