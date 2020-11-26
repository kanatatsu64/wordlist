module Examples.ServerHandler ( list ) where

import System.IO
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Functor.Identity

import Utils ( execCont, contFile )
import CardClass ( Card (..) )
import Csv ( parseRow )
import Html ( Htmlizable (..), template, export )
import Bundle ( Bundle (..) )
import qualified German.Base as German

import Server.Response ( html )

{- Server/Router.hs
    import qualified Examples.ServerHandler ( list )

    router = buildRouter $ do
        get "/list" $ Examples.ServerHandler.list "german" "from text" "resource/German.csv"
-}
list name desc ifname = html <$> buildHtml name desc ifname

buildHtml name desc ifname = execCont $ do
        hifile <- contFile ifname ReadMode

        return $ do
            cards <- execWriterT (loop hifile)
            let bundle = Bundle name desc cards
            return $ (export . template . toHtml) bundle
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
