{-# LANGUAGE BlockArguments #-}

module BundleTest (
    test_all,

    test_toHtml
) where

import Test.Tasty
import Test.Tasty.HUnit

import Html ( Htmlizable (..), export )
import TestUtils ( getCardMock )
import Bundle ( Bundle (..) )

test_all = testGroup "Bundle" [
        test_toHtml
    ]

test_toHtml = testGroup "toHtml" [
        test_toHtml1,
        test_toHtml2
    ]
    where
        test_toHtml1 = testCase "toHtml 1" do
            card <- getCardMock "card mock"
            let bundle = Bundle "german" "" [card]
                expected = "<table>" ++
                               "<caption>german</caption>" ++
                               (export . toHtml) card ++
                           "</table>"
                actual = export $ toHtml bundle
            assertEqual "toHtml simple bundle" expected actual
        test_toHtml2 = testCase "toHtml 2" do
            card1 <- getCardMock "card mock 1"
            card2 <- getCardMock "card mock 2"
            let bundle = Bundle "german" "from textbook" [card1, card2]
                expected = "<table>" ++
                               "<caption>german (from textbook)</caption>" ++
                               (export . toHtml) card1 ++
                               (export . toHtml) card2 ++
                           "</table>"
                actual = export $ toHtml bundle
            assertEqual "toHtml complex bundle with a description" expected actual
