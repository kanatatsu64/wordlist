module Examples.ServerResponse ( list ) where

import System.IO
import Control.Monad.Writer

import Utils ( execCont, contFile )
import Card ( Card (..) )
import Plugin ( Plugin (..) )
import Csv ( parseRow )
import qualified UUID ( getRandom )
import Html ( Htmlizable (..), template, export )
import Bundle ( Bundle (..) )
import qualified Plugins.German.Base as German ( getPlugin )

import Server.Response ( html )

{- 
    get "/list" $ Examples.ServerHandler.list "german" "from text" "resource/German.csv"
-}
list name desc ifname = html =<< buildHtml name desc ifname

buildHtml name desc ifname = execCont $ do
        hifile <- contFile ifname ReadMode

        return $ do
            cards <- execWriterT (loop hifile)
            let bundle = Bundle name desc cards
            return $ (export . template . toHtml) bundle
    where loop :: Handle -> WriterT [Card] IO ()
          loop handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
            then return ()
            else do
                plugin <- liftIO German.getPlugin
                uuid <- liftIO UUID.getRandom
                line <- liftIO $ hGetLine handle
                case (toCard plugin uuid . parseRow) line of
                    Just card -> do
                        tell [card]
                        loop handle
                    Nothing -> fail $ "unable to parse: " ++ line
