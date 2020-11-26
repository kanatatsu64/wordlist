{-# LANGUAGE OverloadedStrings #-}

module Server.Router (
    router,
    responder
) where

import Data.Text ( pack )
import Control.Monad.Writer
import qualified Control.Monad.State as State
import Network.HTTP.Types.Method ( Method, methodGet, methodPost )
import Network.HTTP.Types.URI ( Query )
import Network.Wai ( lazyRequestBody, queryString, pathInfo, requestMethod, Request, Response )

import Server.Types ( Path, Body, Param )
import Server.Handler ( Handler, Handlable (..) )
import Server.Response ( sample, notFound )

router :: Method -> Path -> Query -> Body -> IO Response
router = buildRouter $ do
    get "/" sample
    get "/sample" sample

responder :: Request -> IO Response
responder request = router method path query =<< ioBody
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

get :: (Handlable h) => RawPattern -> h -> Router
get raw h = create $ Route methodGet (buildPath raw) (toHandler h)

post :: (Handlable h) => RawPattern -> h -> Router
post raw h = create $ Route methodPost (buildPath raw) (toHandler h)

buildRouter :: Router -> Method -> Path -> Query -> Body -> IO Response
buildRouter rt method path query body =
    case choose routes method path of
        Just (Route _ _ handler) -> handler body params
        Nothing -> return notFound
    where routes :: [Route]
          routes = runRouter rt
          choose :: [Route] -> Method -> Path -> Maybe Route
          choose [] _ _ = Nothing
          choose (r:rs) method path = if match r method path
                                      then Just r
                                      else choose rs method path
          params :: [Param]
          params = queryToParams query

match :: Route -> Method -> Path -> Bool
match (Route method' path' _) method path = 
    (method' == method) && (path' == path)
