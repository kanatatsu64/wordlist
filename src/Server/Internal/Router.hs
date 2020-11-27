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
import qualified Control.Monad.State as State
import Network.HTTP.Types.Method ( Method, methodGet, methodPost )
import Network.HTTP.Types.URI ( Query )
import Network.Wai ( lazyRequestBody, queryString, pathInfo, requestMethod, Request, Response )

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

data Route = Route {
    method :: Method,
    path :: Path,
    handler :: Handler
}

type Router = Writer [Route] ()

create :: Route -> Router 
create r = tell [r]

runRouter :: Router -> [Route]
runRouter = execWriter

split :: String -> [String]
split str = reverse $ State.execState (loop str) [""]
    where loop :: String -> State.State [String] ()
          loop [] = return ()
          loop ('/':rests) = do
              State.modify ([""] <>)
              loop rests
          loop (x:rests) = do
              addChar x
              loop rests
          addChar :: Char -> State.State [String] ()
          addChar x = do
              css <- State.get
              case css of
                  (c:cs) -> do
                      let c' = c ++ [x]
                      State.put (c':cs)
                  [] -> State.put [[x]]

buildPath :: RawPattern -> Path
buildPath raw = map pack $ filterRoot $ tail $ split raw
    where tail [] = []
          tail (_:xs) = xs
          filterRoot [""] = []
          filterRoot xs = xs

get :: RawPattern -> Handler -> Router
get raw handler = create $ Route methodGet (buildPath raw) handler

post :: RawPattern -> Handler -> Router
post raw handler = create $ Route methodPost (buildPath raw) handler

buildRouter :: Router -> Method -> Path -> Query -> Body -> Request -> IO Response
buildRouter rt method path query body request =
    case choose routes method path of
        Just (Route _ _ (Handler h), rparams) -> (toController h) body (cons rparams qparams) request
        Nothing -> notFound
    where routes :: [Route]
          routes = runRouter rt
          choose :: [Route] -> Method -> Path -> Maybe (Route, [Param])
          choose [] _ _ = Nothing
          choose (r:rs) method path
              | matched = Just (r, params)
              | otherwise = choose rs method path
              where (matched, params) = match r method path
          qparams :: [Param]
          qparams = queryToParams query

match :: Route -> Method -> Path -> (Bool, [Param])
match (Route method' path' _) method path
    | method' == method =
        case runWriter $ matchPath path' path of
            res@(True, _) -> res
            (False, _) -> failure
    | otherwise = failure
    where matchPath :: Path -> Path -> Writer [Param] Bool
          matchPath (t:ts) (p:ps) = case t of
              (unpack -> ':':key) -> do
                  tell [(encode key, Just $ encodeUtf8 p)]
                  matchPath ts ps
              _ | t == p -> matchPath ts ps
              _ | otherwise -> return False
          matchPath [] [] = return True
          matchPath _ _ = return False
          failure :: (Bool, [Param])
          failure = (False, [])
