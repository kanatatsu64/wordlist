module Server.Types (
    lazyEncode,
    Path,
    Body,
    Param
) where

import Data.Text ( Text )
import qualified Data.Text.Lazy ( pack )
import qualified Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy ( ByteString )

type LazyByteString = Data.ByteString.Lazy.ByteString

lazyEncode :: String -> LazyByteString
lazyEncode = Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.pack

type Path = [Text]
type Body = LazyByteString
type Param = (ByteString, Maybe ByteString)
