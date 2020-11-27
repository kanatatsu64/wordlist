module Csv (
    parseRow,

    trim
) where

import Control.Monad.State

import Utils ( split )

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
