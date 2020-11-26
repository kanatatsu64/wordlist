module Utils (
    contFile,
    cont,
    execContT,
    execCont
) where

import System.IO
import Control.Monad.Cont
import Data.Functor.Identity

contFile path mode = cont $ withFile path mode

execContT :: (Monad m) => ContT r m r -> m r
execContT c = runContT c return

execCont :: Cont r r -> r
execCont c = runIdentity $ execContT c
