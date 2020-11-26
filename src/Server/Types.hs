module Server.Types (
    Path,
    Body,
    Param
) where

import Data.Text ( Text )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy ( ByteString )

type LazyByteString = Data.ByteString.Lazy.ByteString

type Path = [Text]
type Body = LazyByteString
type Param = (ByteString, Maybe ByteString)
