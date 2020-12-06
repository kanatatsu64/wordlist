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

import Utils ( for )
import Server.Types ( Path, Body, Param, buildPath, (</>), cons, encode )
import Server.Internal.Handler ( Handler (..), Handlable (..) )
import Server.Response ( notFound )

responder :: Router -> Request -> IO Response
responder = responderRaw . toRouterAsRoot

responderRaw :: RawRouter -> Request -> IO Response
responderRaw router request = parseRequest request (buildRawRouter router)

responderDSL :: RouterDSL -> RawRouter -> Request -> IO Response
responderDSL dsl root = responderRaw $ toRouter dsl root

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

type RawPattern = String

queryToParams :: Query -> [Param]
queryToParams = id

type Pattern = Path

data Route = Route {
    method :: Method,
    pattern :: Pattern,
    handler :: Handler
}

type Router = RouterDSL
type RawRouter = WriterT [Route] IO ()
type RouterDSL = ReaderT RawRouter (WriterT [Route] IO) ()

create :: Route -> RawRouter 
create r = creates [r]

creates :: [Route] -> RawRouter
creates = tell

newRouter :: IO [Route] -> RawRouter
newRouter ioRts = WriterT $ do
    rts <- ioRts
    return ((), rts)

runRouter :: RawRouter -> IO [Route]
runRouter = execWriterT

toRouterAsRoot :: RouterDSL -> RawRouter
toRouterAsRoot dsl = root
    where root = toRouter dsl root

toRouter :: RouterDSL -> RawRouter -> RawRouter
toRouter = runReaderT

fromRouter :: RawRouter -> RouterDSL
fromRouter rt = ReaderT $ const rt

createDSL :: Route -> RouterDSL
createDSL = fromRouter . create

createsDSL :: [Route] -> RouterDSL
createsDSL = fromRouter. creates

newRouterDSL :: (RawRouter -> IO [Route]) -> RouterDSL
newRouterDSL k = ReaderT $ \root -> newRouter (k root)

runRouterDSL :: RouterDSL -> RawRouter -> IO [Route]
runRouterDSL dsl root = execWriterT $ runReaderT dsl root

get :: RawPattern -> Handler -> RouterDSL
get raw handler = do
    path <- liftIO $ buildPath raw
    createDSL $ Route methodGet path handler

post :: RawPattern -> Handler -> RouterDSL
post raw handler = do
    path <- liftIO $ buildPath raw
    createDSL $ Route methodPost path handler

type RoutePattern = Handler -> RouterDSL

parsePattern :: RoutePattern -> RawRouter -> IO (Method, Path)
parsePattern pat root = do
    [Route method path _] <- runRouterDSL (pat undefined) root
    return (method, path)

(~>) :: RoutePattern -> RoutePattern -> RouterDSL
src ~> dst = newRouterDSL $ \root -> do
    (ms, ps) <- parsePattern src root
    (md, pd) <- parsePattern dst root
    let handler = redirect root md pd
    return [Route ms ps handler]

redirect :: RawRouter -> Method -> Path -> Handler
redirect root method path = Handler $ \request -> do
        mres <- runMaybeT $ selectRoute root method path
        case mres of
            Just (route, rparams) -> do
                let responder = createResponder route rparams
                responder request
            Nothing -> notFound

mount :: RawPattern -> RouterDSL -> RouterDSL
mount base dsl = newRouterDSL $ \root -> do
        path <- buildPath base
        validate path
        routes <- runRouterDSL dsl root
        let routes' = for routes $ \(Route m p h) -> Route m (path </> p) h
        return routes'
    where validate path
              | Text.length (last path) == 0 = return ()
              | head (last path) == '*' = fail $ "invalid base pattern: " ++ base
              | otherwise = return ()

buildRawRouter :: RawRouter -> Method -> Path -> Query -> Body -> Request -> IO Response
buildRawRouter rt method path query body request = do
    mres <- runMaybeT $ selectRoute rt method path
    case mres of
        Just (Route _ _ (Handler h), rparams) -> (toController h) body (cons rparams qparams) request
        Nothing -> notFound
    where qparams :: [Param]
          qparams = queryToParams query

buildRouterDSL :: RouterDSL -> RawRouter -> Method -> Path -> Query -> Body -> Request -> IO Response
buildRouterDSL dsl root = buildRawRouter $ toRouter dsl root

selectRoute :: RawRouter -> Method -> Path -> MaybeT IO (Route, [Param])
selectRoute rt method path = do
        routes <- liftIO $ runRouter rt
        choose routes method path
    where choose :: [Route] -> Method -> Path -> MaybeT IO (Route, [Param])
          choose [] _ _ = MaybeT $ return Nothing
          choose (r:rs) method path
              | matched = return (r, params)
              | otherwise = choose rs method path
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
