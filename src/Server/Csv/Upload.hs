module Server.Csv.Upload (
    getHandler,
    postHandler
) where

import Network.Wai.Parse ( FileInfo (..) )

import Server.Types ( lazyDecode, decode )
import Server.Handler ( htmlFile, uploader )

store :: String -> String -> IO ()
store name = writeFile ("resource/" ++ name)

getHandler = htmlFile "client/CsvUpload.html"

postHandler = uploader $ \(_, info) ->
    store (name info) (content info)
    where name = decode . fileName
          content = lazyDecode . fileContent
