module CardClass (
    Card (..),
    Language (..),
    CardID,
    fromCsv
) where

import System.IO
import Control.Monad.Writer

import Csv ( parseRow )
import Utils ( contFile, execCont )

data Language = Japanese | English | Chinese | French | German

type CardID = String

class Card a where
    language :: a -> Language
    cardid :: a -> CardID
    word :: a -> String
    meaning :: a -> String
    toCard :: [String] -> a

fromCsv :: Card a => FilePath -> IO [a]
fromCsv path = execCont $ do
        hcsv <- contFile path ReadMode
        return $ execWriterT (loop hcsv)
    where loop :: (Card a) => Handle -> WriterT [a] IO ()
          loop handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
            then return ()
            else do
                line <- liftIO $ hGetLine handle
                let card = (toCard . parseRow) line
                tell [card]
                loop handle
