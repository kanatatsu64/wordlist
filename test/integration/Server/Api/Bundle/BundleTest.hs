{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.BundleTest (
    test_all,

    test_createHandler,
    test_updateHandler,
    test_getHandler,
    test_deleteHandler,
    test_addHandler
) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai.Test

import Convertible ( Convertible (..), convert )
import Server.Json ( JObj, jsonify, Json (..), Dict (..), Rec (..), Ary (..) )
import Server.Bundle ( Bundle (..) )
import qualified Server.Bundle as Bundle
import qualified Server.Card as Card
import TestUtils ( runAppTest, getCardMock, getBundleMock, compile, clean, getTestUUID, getTestUUID2, assertBundleEqual )
import Server.Api.Bundle.Bundle ( createHandler, updateHandler, getHandler, deleteHandler, addHandler )

test_all = testGroup "Bundle" [
        test_createHandler,
        test_updateHandler,
        test_getHandler,
        test_deleteHandler,
        test_addHandler
    ]

test_createHandler = testCase "createHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test"
    Card.save card

    muuid <- runAppTest app do
        let Json body = jsonify $ Dict [
                   Rec "bundleid" uuid,
                   Rec @String "name" "test name",
                   Rec @String "desc" "test desc",
                   Rec "cardids" (Ary [uuid])
                ]
            sreq = SRequest defaultRequest (convert body)
        sres <- srequest sreq
        assertStatus 200 sres
        assertContentType "application/json" sres
        let str = convert . simpleBody $ sres
            json = Json str
            muuid = safeConvert json >>= safeConvert @JObj
        return muuid

    let msg = "returned UUID is invalid"
    buuid <- case muuid of
        Right val -> do
            assertBool msg (uuid /= val)
            return val
        Left _ -> assertFailure msg
    let expected = Bundle buuid "test name" "test desc" [card]
    actual <- Bundle.load buuid 
    assertBundleEqual "bundle is not correctly saved" expected actual
    where app = compile [] createHandler

test_updateHandler = testCase "updateHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test card"
    Card.save card
    bundle <- getBundleMock uuid "test name" [card]
    Bundle.save bundle 

    uuid2 <- getTestUUID2
    card2 <- getCardMock uuid2 "changed card"
    Card.save card2

    runAppTest app do
        let Json body = jsonify $ Dict [
                    Rec "bundleid" uuid,
                    Rec @String "name" "changed name",
                    Rec @String "desc" "changed desc",
                    Rec "cardids" (Ary [uuid2])
                ]
            sreq = SRequest defaultRequest (convert body)
        sres <- srequest sreq
        assertStatus 200 sres

    let expected = Bundle uuid "changed name" "changed desc" [card2]
    actual <- Bundle.load uuid 
    assertBundleEqual "bundle is not correctly updated" expected actual
    where app = compile [] updateHandler

test_getHandler = testCase "getHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test card"
    bundle <- getBundleMock uuid "test name" [card]
    Bundle.save bundle

    let app = compile [("id", Just $ convert uuid)] getHandler
    mbundle <- runAppTest app do
        sres <- request defaultRequest
        assertStatus 200 sres
        assertContentType "application/json" sres
        let str = convert . simpleBody $ sres
            json = Json str
            mbundle = safeConvert json >>= safeConvert @JObj
        return mbundle

    let expected = bundle
        msg = "returned bundle is incorrect"
    case mbundle of
        Right actual -> assertBundleEqual msg expected actual
        Left _ -> assertFailure msg

test_deleteHandler = testCase "deleteHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test card"
    bundle <- getBundleMock uuid "test name" [card]
    Bundle.save bundle

    let app = compile [("id", Just $ convert uuid)] deleteHandler
    runAppTest app do
        sres <- request defaultRequest
        assertStatus 200 sres
    
    exist <- Bundle.exist uuid
    assertBool "bundle is not deleted" (not exist)

test_addHandler = testCase "addHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test card"
    bundle <- getBundleMock uuid "test name" [card]
    Bundle.save bundle

    uuid2 <- getTestUUID2
    card2 <- getCardMock uuid2 "test card2"
    Card.save card2

    let app = compile [("id", Just $ convert uuid)] addHandler
    runAppTest app do
        let Json body = jsonify $ Ary [uuid2]
            sreq = SRequest defaultRequest (convert body)
        sres <- srequest sreq
        assertStatus 200 sres

    actual <- Bundle.load uuid    
    let expected = bundle { cards = [card, card2] }
        msg = "cards are not correctly added"
    assertBundleEqual msg expected actual
