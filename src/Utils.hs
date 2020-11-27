module Utils (
    contFile,
    cont,
    execContT,
    execCont,
    split
) where

import System.IO
import Control.Monad.Cont
import Control.Monad.State
import Data.Functor.Identity

contFile path mode = cont $ withFile path mode

execContT :: (Monad m) => ContT r m r -> m r
execContT c = runContT c return

execCont :: Cont r r -> r
execCont c = runIdentity $ execContT c

split :: Char -> String -> [String]
split delim str = reverse $ execState (loop str) [""]
    where loop :: String -> State [String] ()
          loop [] = return ()
          loop (x:rests)
              | x == delim = do
                  modify ([""] <>)
                  loop rests
              | otherwise = do
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
