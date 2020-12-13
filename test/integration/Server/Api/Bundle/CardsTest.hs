{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Bundle.CardsTest (
    test_all,

    test_addHandler,
    test_deleteHandler
) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai.Test

import Convertible ( convert )
import qualified Server.Bundle as Bundle
import qualified Server.Card as Card
import Server.Bundle ( Bundle (..) )
import Server.Json ( Json (..), Ary (..), jsonify )
import TestUtils ( runAppTest, compile, clean, getTestUUID, getTestUUID2, getBundleMock, getCardMock, assertBundleEqual )
import Server.Api.Bundle.Cards ( addHandler, deleteHandler )

test_all = testGroup "Cards" [
        test_addHandler,
        test_deleteHandler
    ]

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

test_deleteHandler = testCase "deleteHandler" do
    clean
    uuid <- getTestUUID
    card <- getCardMock uuid "test card"
    uuid2 <- getTestUUID2
    card2 <- getCardMock uuid2 "test card2"
    bundle <- getBundleMock uuid "test name" [card, card2]
    Bundle.save bundle

    let app = compile [("id", Just $ convert uuid)] deleteHandler
    runAppTest app do
        let Json body = jsonify $ Ary [uuid2]
            sreq = SRequest defaultRequest (convert body)
        sres <- srequest sreq
        assertStatus 200 sres

    actual <- Bundle.load uuid    
    let expected = bundle { cards = [card] }
        msg = "cards are not correctly deleted"
    assertBundleEqual msg expected actual
