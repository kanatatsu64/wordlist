{-# LANGUAGE ViewPatterns #-}

module Server.Internal.Router (
    responder,
    get,
    post,
    Router (..)
) where

import Prelude hiding ( head, tail )

import Data.Text ( pack, unpack )
import Data.Text.Encoding ( encodeUtf8 )
import Control.Monad.Writer
import Network.HTTP.Types.Method ( Method, methodGet, methodPost )
import Network.HTTP.Types.URI ( Query )
import Network.Wai ( lazyRequestBody, queryString, pathInfo, requestMethod, Request, Response )

import Utils ( split )
import Server.Types ( Path, Body, Param, cons, encode )
import Server.Internal.Handler ( Handler (..), Handlable (..) )
import Server.Response ( notFound )

responder :: Router -> Request -> IO Response
responder router request = do
        body <- ioBody
        buildRouter router method path query body request
    where method = requestMethod request
          path = pathInfo request
          query = queryString request
          ioBody = lazyRequestBody request

type RawPattern = String

queryToParams :: Query -> [Param]
queryToParams = id

type Pattern = Path

data Route = Route {
    method :: Method,
    pattern :: Pattern,
    handler :: Handler
}

type Router = WriterT [Route] IO ()

create :: Route -> Router 
create r = tell [r]

runRouter :: Router -> IO [Route]
runRouter = execWriterT

buildPath :: RawPattern -> IO Path
buildPath raw = map pack <$> (validate . filterRoot . tail) (split '/' raw)
    where tail [] = []
          tail (_:xs) = xs
          filterRoot [""] = []
          filterRoot xs = xs
          validate [] = return []
          validate ps@[_] = return ps
          validate (p:ps) = case p of
              '*':_ -> fail $ "invalid pattern: " ++ raw
              _ -> (p:) <$> validate ps

get :: RawPattern -> Handler -> Router
get raw handler = do
    path <- liftIO $ buildPath raw
    create $ Route methodGet path handler

post :: RawPattern -> Handler -> Router
post raw handler = do
    path <- liftIO $ buildPath raw
    create $ Route methodPost path handler

buildRouter :: Router -> Method -> Path -> Query -> Body -> Request -> IO Response
buildRouter rt method path query body request = do
    routes <- ioRoutes
    case choose routes method path of
        Just (Route _ _ (Handler h), rparams) -> (toController h) body (cons rparams qparams) request
        Nothing -> notFound
    where ioRoutes :: IO [Route]
          ioRoutes = runRouter rt
          choose :: [Route] -> Method -> Path -> Maybe (Route, [Param])
          choose [] _ _ = Nothing
          choose (r:rs) method path
              | matched = Just (r, params)
              | otherwise = choose rs method path
              where (matched, params) = match r method path
          qparams :: [Param]
          qparams = queryToParams query

match :: Route -> Method -> Path -> (Bool, [Param])
match (Route method' pattern' _) method path
    | method' == method =
        case runWriter $ matchPath pattern' path of
            res@(True, _) -> res
            (False, _) -> failure
    | otherwise = failure
    where matchPath :: Pattern -> Path -> Writer [Param] Bool
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
          matchPath [] [] = return True
          matchPath _ _ = return False
          failure :: (Bool, [Param])
          failure = (False, [])
