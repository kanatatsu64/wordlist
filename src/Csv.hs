module Csv (
    parseRow,

    split,
    trim
) where

import Control.Monad.State

parseRow :: String -> [String]
parseRow row = map trim (split row)

split :: String -> [String]
split str = reverse $ execState (loop str) [""]
    where loop :: String -> State [String] ()
          loop [] = return ()
          loop (',':rests) = do
              modify ([""] <>)
              loop rests
          loop (x:rests) = do
              addChar x
              loop rests
          addChar :: Char -> State [String] ()
          addChar x = do
              css <- get
              case css of
                  (c:cs) -> do
                      let c' = c ++ [x]
                      put (c':cs)
                  [] -> put [[x]]

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
