{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Card.CardTest (
    test_all,

    test_getHandler,
    test_createHandler,
    test_updateHandler,
    test_deleteHandler
) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai.Test

import qualified Server.Card as Card
import Server.Card ( Card (..) )
import Convertible ( convert, safeConvert )
import Server.Json ( Json (..), JObj, jsonify )
import TestUtils ( runAppTest, compile, clean, getTestUUID, getCardMock, assertCardEqual )
import Server.Api.Card.Card ( getHandler, createHandler, updateHandler, deleteHandler )

test_all = testGroup "Card" [
        test_getHandler,
        test_createHandler,
        test_updateHandler,
        test_deleteHandler
    ]

test_getHandler = testCase "getHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test word"
    Card.save card

    let app = compile [("id", Just $ convert uuid)] getHandler
    mcard <- runAppTest app do
        sres <- request defaultRequest
        assertStatus 200 sres
        assertContentType "application/json" sres
        let str = convert . simpleBody $ sres
            json = Json str
            mcard = safeConvert json >>= safeConvert @JObj
        return mcard

    let expected = card
        msg = "returned card is incorrect"
    case mcard of
        Right actual -> assertCardEqual msg expected actual
        Left _ -> assertFailure msg

test_createHandler = testCase "createHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test word"

    muuid <- runAppTest app do
        let Json body = jsonify card
            sreq = SRequest defaultRequest (convert body)
        sres <- srequest sreq
        assertStatus 200 sres
        assertContentType "application/json" sres
        let str = convert . simpleBody $ sres
            json = Json str
            muuid = safeConvert json >>= safeConvert @JObj
        return muuid

    let msg = "returned UUID is invalid"
    cuuid <- case muuid of
        Right val -> do
            assertBool msg (val /= uuid)
            return val
        Left _ -> assertFailure msg
    
    let expected = card { cardid = cuuid }
    actual <- Card.load cuuid
    assertCardEqual "card is not correctly saved" expected actual
    where app = compile [] createHandler

test_updateHandler = testCase "updateHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test word"
    Card.save card

    let card2 = card { word = "changed word", meaning = "changed meaning" }

    runAppTest app do
        let Json body = jsonify card2
            sreq = SRequest defaultRequest (convert body)
        sres <- srequest sreq
        assertStatus 200 sres
    
    let expected = card2
        msg = "card is not correctly updated"
    actual <- Card.load uuid
    assertCardEqual msg expected actual
    where app = compile [] updateHandler

test_deleteHandler = testCase "deleteHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test word"
    Card.save card

    let app = compile [("id", Just $ convert uuid)] deleteHandler
    runAppTest app do
        sres <- request defaultRequest
        assertStatus 200 sres
    
    exist <- Card.exist uuid
    assertBool "card is not deleted" (not exist)
