module Csv (
    parseRow,
    parseCsv,
    loadCsv,

    trim
) where

import System.IO
import Control.Monad.State

import Card ( Card )
import Plugin ( Plugin (..) )
import UUID ( getRandom )
import Utils ( split, contFile, execCont, for )

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

parseCsv :: Plugin -> [String] -> IO [Card]
parseCsv plugin lines = sequence $ for lines $ \line -> do
    uuid <- getRandom
    case (toCard plugin uuid. parseRow) line of
        Just card -> return card
        Nothing -> fail $ "Unable to parse: " ++ line

loadCsv :: Plugin -> FilePath -> IO [Card]
loadCsv plugin path = execCont $ do
        hcsv <- contFile path ReadMode
        return $ do
            contents <- hGetContents hcsv
            parseCsv plugin (lines contents)
