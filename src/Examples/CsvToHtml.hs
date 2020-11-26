module Examples.CsvToHtml (
    csvToHtml
) where

import System.IO
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Functor.Identity

import CardClass ( Card (..) )
import Csv ( parseRow )
import Html ( Htmlizable (..), template, export )
import Bundle ( Bundle (..) )
import qualified German.Base as German

execContT :: (Monad m) => ContT r m r -> m r
execContT c = runContT c return

execCont :: Cont r r -> r
execCont c = runIdentity $ execContT c

{-
    import Examples.CsvToHtml

    main = csvToHtml name desc csv html
        where name = "list"
            desc = "sample"
            csv = "resource/German.csv"
            html = "resource/German.html"

-}
csvToHtml name desc ifname ofname = execCont $ do
        hifile <- cont $ withFile ifname ReadMode
        hofile <- cont $ withFile ofname WriteMode

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
