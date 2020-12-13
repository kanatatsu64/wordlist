{-# LANGUAGE RankNTypes #-}

module TestUtils (
    getTestUUID,
    getTestUUID2,
    getTestUUID3,
    getBundleMock,
    getCardMock,
    runWebTest,
    runAppTest,
    compile,
    clean,
    assertBundleEqual,
    assertCardEqual
) where

import Test.Tasty.HUnit
import Network.Wai.Test ( Session, runSession )
import Network.Wai ( Application )
import Data.Sort ( sort, sortOn )
import Control.Monad

import UUID ( UUID, fromString )
import Serial ( Serial (..), serialize )
import Types ( Example (..), Language (..) )
import Utils ( field )
import Server.SQL ( execRuntime, ISchema (..), IConnection, Runtime, runDeleteAll )
import Server.Bundle ( Bundle (..), BundleSchema, BundleToCardSchema )
import Server.Card ( Card (..), CardSchema, AttrSchema, ExampleSchema )
import Server.Application ( app )
import Server.Internal.Handler ( Handler (..), toController )
import Server.Internal.Router ( parseRequest )
import Server.Types ( Param, cons )

getTestUUID :: IO UUID
getTestUUID = fromString "00000000-0000-0000-0000-000000000000"

getTestUUID2 :: IO UUID
getTestUUID2 = fromString "00000000-0000-0000-0000-000000000001"

getTestUUID3 :: IO UUID
getTestUUID3 = fromString "00000000-0000-0000-0000-000000000002"

getBundleMock :: UUID -> String -> [Card] -> IO Bundle
getBundleMock _bundleid _name _cards = do
    return $ Bundle _bundleid _name "test desc" _cards

getCardMock :: UUID -> String -> IO Card
getCardMock _cardid _word = do
    _pluginid <- getTestUUID
    let _language = German
        _meaning = "test meaning"
        _attrs = [Serial "test attribute"]
        _note = "test note"
        _examples = [Example "test original" "test translation"]
    return $ Card _cardid _pluginid _language _word _meaning _attrs _note _examples

runWebTest :: Session a -> IO a
runWebTest s = runSession s app

runAppTest :: Application -> Session a -> IO a
runAppTest a s = runSession s a

compile :: [Param] -> Handler -> Application
compile params (Handler h) = \request respond ->
    respond =<< parseRequest request callback
    where callback = \_ _ _ body request ->
            toController h body params request

clean :: IO ()
clean = void $ execRuntime do
    field @BundleSchema go
    field @BundleToCardSchema go
    field @CardSchema go
    field @AttrSchema go
    field @ExampleSchema go
    where go :: (ISchema schema, IConnection conn) => schema -> Runtime conn ()
          go schema = void $ runDeleteAll (table schema)

map2M_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
map2M_ f as bs = mapM_ (uncurry f) (zip as bs)

mapSortedOnM_ :: (Ord b, Monad m) => (a -> b) -> (a -> a -> m c) -> [a] -> [a] -> m ()
mapSortedOnM_ g f as bs = map2M_ f (sortOn g as) (sortOn g bs)

mapSortedM_ :: (Ord a, Monad m) => (a -> a -> m c) -> [a] -> [a] -> m ()
mapSortedM_ f as bs = map2M_ f (sort as) (sort bs)

assertBundleEqual :: String -> Bundle -> Bundle -> IO ()
assertBundleEqual msg expected actual = do
    assertEqual ("[bundleid]: " ++ msg) (bundleid expected) (bundleid actual)
    assertEqual ("[name]: " ++ msg) (name expected) (name actual)
    assertEqual ("[desc]: " ++ msg) (desc expected) (desc actual)
    assertBool msg (length (cards expected) == length (cards actual))
    mapSortedOnM_ cardid (assertCardEqual ("[cards]: " ++ msg)) (cards expected) (cards actual)

instance Show Serial where
    show = serialize

instance Eq Serial where
    a == b = (serialize a) == (serialize b)

assertCardEqual :: String -> Card -> Card -> IO ()
assertCardEqual msg expected actual = do
    assertEqual ("[cardid]: " ++ msg) (cardid expected) (cardid actual)
    assertEqual ("[pluginid]: " ++ msg) (pluginid expected) (pluginid actual)
    assertEqual ("[word]: " ++ msg) (word expected) (word actual)
    assertEqual ("[meaning]: " ++ msg) (meaning expected) (meaning actual)
    assertEqual ("[attrs]: " ++ msg) (attrs expected) (attrs actual)
    assertEqual ("[note]: " ++ msg) (note expected) (note actual)
    assertBool msg (length (examples expected) == length (examples actual))
    mapSortedOnM_ original (assertExampleEqual ("[examples]" ++ msg)) (examples expected) (examples actual)
    where assertExampleEqual msg expected actual = do
            assertEqual ("[original]: " ++ msg) (original expected) (original actual)
            assertEqual ("[translation]: " ++ msg) (translation expected) (translation actual)
