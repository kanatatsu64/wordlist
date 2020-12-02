module Examples.CsvToHtml (
    csvToHtml
) where

import System.IO
import Control.Monad.Writer

import Utils ( execCont, contFile )
import Card ( Card (..) )
import qualified UUID ( getRandom )
import Plugin ( Plugin (..) )
import Csv ( parseRow )
import Html ( Htmlizable (..), template, export )
import Bundle ( Bundle (..) )
import qualified Plugins.German.Base as German ( getPlugin )

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
    where loop :: Handle -> WriterT [Card] IO ()
          loop handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
            then return ()
            else do
                plugin <- German.getPlugin
                uuid <- liftIO UUID.getRandom
                line <- liftIO $ hGetLine handle
                case (toCard plugin uuid. parseRow) line of
                    Just card -> do
                        tell [card]
                        loop handle
                    Nothing -> fail $ "unable to parse: " ++ line
