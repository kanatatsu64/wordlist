{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Utils (
    contFile,
    cont,
    execContT,
    execCont,
    split,
    maybeToFail,
    for,
    push,
    (<@>),
    (<||>),
    withIndex,
    cons,
    shrink,
    same,
    contentEqual,
    lines
) where

import Prelude hiding ( lines )
import qualified Prelude ( lines )

import System.IO
import Control.Monad.Cont
import Control.Monad.State
import Data.Functor.Identity
import Data.Sort

instance MonadFail (Either String) where
    fail = Left

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

maybeToFail :: MonadFail m => String -> Maybe a -> m a
maybeToFail _ (Just x) = return x
maybeToFail msg Nothing = fail msg

for :: [a] -> (a -> b) -> [b]
for = flip map

push :: a -> [a] -> [a]
push x xs = xs ++ [x]

infixl 4 <@>

(<@>) :: Applicative f => f (a -> b) -> a -> f b
(<@>) h x = ($ x) <$> h

infixl 3 <||>

(<||>) :: [a] -> [a] -> [a]
[] <||> bs = bs
as <||> _ = as

withIndex :: [a] -> [(Integer, a)]
withIndex = zip [0..]

cons :: Monoid a => (a -> a -> a) -> [a] -> a
cons _ [] = mempty
cons _ [x] = x
cons f (x:xs) = f x (cons f xs)

shrink :: (a -> a -> b) -> [a] -> [b]
shrink _ [] = []
shrink _ [_] = []
shrink f (x:y:rs) = f x y:shrink f (y:rs)

same :: Eq a => [a] -> Bool
same = and . shrink (==)

contentEqual :: (Eq a, Ord a) => [a] -> [a] -> Bool
contentEqual xs ys = sort xs == sort ys

lines :: String -> [String]
lines str = for (Prelude.lines str) $ \line -> trim line
    where trim line@(last -> '\r') = init line
          trim line = line
