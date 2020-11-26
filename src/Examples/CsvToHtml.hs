module Examples.CsvToHtml (
    csvToHtml
) where

import System.IO
import Control.Monad.Writer

import Utils ( execCont, contFile )
import CardClass ( Card (..) )
import Csv ( parseRow )
import Html ( Htmlizable (..), template, export )
import Bundle ( Bundle (..) )
import qualified German.Base as German

{-
    import Examples.CsvToHtml

    main = csvToHtml name desc csv html
        where name = "list"
            desc = "sample"
            csv = "resource/German.csv"
            html = "resource/German.html"

-}
csvToHtml name desc ifname ofname = execCont $ do
        hifile <- contFile ifname ReadMode
        hofile <- contFile ofname WriteMode

        return $ do
            cards <- execWriterT (loop hifile)
            let bundle = Bundle name desc cards
            hPutStr hofile $ (export . template . toHtml) bundle
    where loop :: Handle -> WriterT [German.Card] IO ()
          loop handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
            then return ()
            else do
                line <- liftIO $ hGetLine handle
                let card = (toCard . parseRow) line
                tell [card]
                loop handle
