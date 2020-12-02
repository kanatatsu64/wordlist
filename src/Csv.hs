module Csv (
    parseRow,
    loadCsv,

    trim
) where

import System.IO
import Control.Monad.Writer
import Control.Monad.State

import Card ( Card )
import Plugin ( Plugin (..) )
import UUID ( getRandom )
import Utils ( split, contFile, execCont )

parseRow :: String -> [String]
parseRow row = map trim (split ',' row)

trim :: String -> String
trim str = evalState (loop str) False
    where loop :: String -> State Bool String
          loop [] = return ""
          loop (' ':rests) = do
              {- if proceding spaces are trimmed -}
              flag <- get
              if flag
              then do
                  tail <- loop rests
                  case tail of
                      "" -> return ""
                      _ -> return (' ':tail)
              else loop rests
          loop (x:rests) = do
              put True
              tail <- loop rests
              return (x:tail)

loadCsv :: Plugin -> FilePath -> IO [Card]
loadCsv plugin path = execCont $ do
        hcsv <- contFile path ReadMode
        return $ execWriterT (loop hcsv)
    where loop :: Handle -> WriterT [Card] IO ()
          loop handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
            then return ()
            else do
                line <- liftIO $ hGetLine handle
                uuid <- liftIO getRandom
                case (toCard plugin uuid. parseRow) line of
                    Just card -> do
                        tell [card]
                        loop handle
                    Nothing -> fail $ "Unable to parse: " ++ line
