{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.ListTest (
    test_all,

    test_getNameList
) where

import Prelude hiding ( lookup )
import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai.Test
import Control.Monad
import Data.Sort ( sortOn )

import Convertible ( convert, safeConvert )
import Server.Bundle ( getInfo )
import qualified Server.Bundle as Bundle
import qualified Server.Card as Card
import Server.Json ( Json (..), JObj (..), lookup )
import TestUtils ( runAppTest, compile, clean, getTestUUID, getBundleMock, getCardMock )
import Server.Api.Bundle.List ( getNameList )

test_all = testGroup "List" [
        test_getNameList
    ]

test_getNameList = testCase "getNameList" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test card1"
    Card.save card
    bundle <- getBundleMock uuid "test1" [card]
    Bundle.save bundle

    uuid2 <- getTestUUID
    card2 <- getCardMock uuid2 "test card2"
    Card.save card2
    bundle2 <- getBundleMock uuid2 "test2" [card2]
    Bundle.save bundle2

    mjobj <- runAppTest app do
        sres <- request defaultRequest
        assertStatus 200 sres
        assertContentType "application/json" sres
        let str = convert . simpleBody $ sres
            json = Json str
            mjobj = safeConvert json
        return mjobj
    
    let expected = [getInfo bundle, getInfo bundle2]
        msg = "returned infos are incorrect"
    case mjobj of
        Right (JAry ary) -> do
            actual <- forM ary $ \(JDict jrecs) -> do
                    let minfo = do
                            bundleid <- lookup "bundleid" jrecs undefined
                            name <- lookup "name" jrecs undefined
                            desc <- lookup "desc" jrecs undefined
                            return (bundleid, name, desc)
                    case minfo of
                        Right val -> return val
                        Left _ -> assertFailure msg
            assertInfosEqual msg expected actual
        _ -> assertFailure msg
    where app = compile [] getNameList
          assertInfosEqual msg expected actual =
              let _fst (a, _, _) = a
                  _expected = sortOn _fst expected
                  _actual = sortOn _fst actual
              in assertEqual msg _expected _actual
