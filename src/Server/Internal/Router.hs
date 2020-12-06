{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Internal.Router (
    responder,
    (~>),
    mount,
    get,
    post,
    Router (..)
) where

import Prelude hiding ( head, tail )

import Data.Text ( unpack, head )
import qualified Data.Text as Text ( length )
import Data.Text.Encoding ( encodeUtf8 )
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Reader
import Network.HTTP.Types.Method ( Method, methodGet, methodPost )
import Network.HTTP.Types.URI ( Query )
import Network.Wai ( lazyRequestBody, queryString, pathInfo, requestMethod, Request, Response )

import Server.Types ( Path, Body, Param, queryToParams, buildPath, (</>), cons, encode )
import Server.Internal.Handler ( Handler (..), Handlable (..) )
import Server.Response ( notFound )

type RawPattern = String
type Pattern = Path

data Route = Route {
    method :: Method,
    pattern :: Pattern,
    handler :: Handler
}

type Router = [Route]
type Root = IO [Route]

data Env = Env {
    env_base :: Path,
    env_root :: Root
}
type RouterDSL a = ReaderT Env (WriterT Router IO) a

add :: Route -> RouterDSL ()
add (Route m p h) = do
    base <- asks env_base
    tell [Route m (base </> p) h]

cd :: Path -> RouterDSL () -> RouterDSL ()
cd path dsl = local go dsl
    where go (Env base router) = Env (base </> path) router

fetch :: RouterDSL () -> RouterDSL Router
fetch dsl = do
    env <- getEnv
    router <- liftIO $ runDSL dsl env
    return router

getEnv :: RouterDSL Env
getEnv = ask

getBase :: RouterDSL Path
getBase = asks env_base

getRoot :: RouterDSL Root
getRoot = asks env_root

runDSL :: RouterDSL () -> Env -> Root
runDSL dsl env = execWriterT $ runReaderT dsl env

buildRouter :: RouterDSL () -> Root
buildRouter dsl = root
    where root = runDSL dsl (Env [] root)

responder :: RouterDSL () -> Request -> IO Response
responder dsl request = (parseRequest request . parseRoot . buildRouter) dsl

parseRoot :: Root -> Method -> Path -> Query -> Body -> Request -> IO Response
parseRoot root method path query body request = do
    router <- root
    mres <- runMaybeT $ selectRoute router method path
    case mres of
        Just (Route _ _ (Handler h), rparams) -> (toController h) body (cons rparams qparams) request
        Nothing -> notFound
    where qparams :: [Param]
          qparams = queryToParams query

parseRequest :: MonadIO m =>
                Request ->
                (Method -> Path -> Query -> Body -> Request -> m a) ->
                m a
parseRequest request callback = do
        body <- liftIO $ ioBody
        callback method path query body request
    where method = requestMethod request
          path = pathInfo request
          query = queryString request
          ioBody = lazyRequestBody request

createResponder :: Route -> [Param] -> Request -> IO Response
createResponder (Route _ _ (Handler h)) params = flip parseRequest $
    \_ _ query body request -> (toController h) body (cons params (queryToParams query)) request

selectRoute :: Router -> Method -> Path -> MaybeT IO (Route, [Param])
selectRoute [] _ _ = MaybeT $ return Nothing
selectRoute (r:rs) method path
    | matched = return (r, params)
    | otherwise = selectRoute rs method path
    where (matched, params) = match r method path

match :: Route -> Method -> Path -> (Bool, [Param])
match (Route method' pattern' _) method path
    | method' == method =
        case runWriter $ matchPath pattern' path of
            res@(True, _) -> res
            (False, _) -> failure
    | otherwise = failure
    where matchPath :: Pattern -> Path -> Writer [Param] Bool
          matchPath [] [] = return True
          matchPath ("":ts) ps = matchPath ts ps
          matchPath ts ("":ps) = matchPath ts ps
          matchPath (t:ts) path@(p:ps) = case t of
              (unpack -> ':':key) -> do
                  tell [(encode key, Just $ encodeUtf8 p)]
                  matchPath ts ps
              (unpack -> "*") -> do
                  tell [(encode "path", Just $ encodeUtf8 $ mconcat path)]
                  matchPath ts ps
              (unpack -> '*':key) -> do
                  tell [(encode key, Just $ encodeUtf8 $ mconcat path)]
                  matchPath ts ps
              _ | t == p -> matchPath ts ps
              _ | otherwise -> return False
          matchPath _ _ = return False
          failure :: (Bool, [Param])
          failure = (False, [])

get :: RawPattern -> Handler -> RouterDSL ()
get raw handler = do
    path <- liftIO $ buildPath raw
    add $ Route methodGet path handler

post :: RawPattern -> Handler -> RouterDSL ()
post raw handler = do
    path <- liftIO $ buildPath raw
    add $ Route methodPost path handler

type RoutePattern = Handler -> RouterDSL ()

(~>) :: RoutePattern -> RoutePattern -> RouterDSL ()
src ~> dst = do
    [Route ms ps _] <- fetch $ src undefined
    [Route md pd _] <- fetch $ dst undefined
    handler <- redirect md pd
    add $ Route ms ps handler

redirect :: Method -> Path -> RouterDSL Handler
redirect method path = do
    root <- getRoot
    return $ Handler $ \request -> do
        router <- root
        mres <- runMaybeT $ selectRoute router method path
        case mres of
            Just (route, rparams) -> do
                let responder = createResponder route rparams
                responder request
            Nothing -> notFound

mount :: RawPattern -> RouterDSL () -> RouterDSL ()
mount base dsl = do
    path <- liftIO $ buildPath base
    validate path
    cd path dsl
    where
        validate path
            | Text.length (last path) == 0 = return ()
            | head (last path) == '*' = fail $ "invalid base pattern: " ++ base
            | otherwise = return ()
