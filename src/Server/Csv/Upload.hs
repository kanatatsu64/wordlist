module Server.Csv.Upload (
    postHandler
) where

import Network.Wai.Parse ( FileInfo (..) )

import Server.Types ( lazyDecode, decode )
import Server.Handler ( uploader )

store :: String -> String -> IO ()
store name = writeFile ("resource/" ++ name)

postHandler = uploader $ \(_, info) ->
    store (name info) (content info)
    where name = decode . fileName
          content = lazyDecode . fileContent
