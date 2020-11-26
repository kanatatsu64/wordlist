{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BundleTest (
    test_all,

    test_toHtml
) where

import Test.Tasty
import Test.Tasty.HUnit

import Html ( Htmlizable (..), export )
import Utils ( CardMock (..) )
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
            let card = CardMock "card mock"
            let bundle = Bundle "german" "" [card]
            let expected = "<table>" ++
                               "<caption>german</caption>" ++
                               (export . toHtml) card ++
                           "</table>"
            let actual = export $ toHtml bundle
            assertEqual "toHtml simple bundle" expected actual
        test_toHtml2 = testCase "toHtml 2" do
            let card1 = CardMock "card mock 1"
            let card2 = CardMock "card mock 2"
            let bundle = Bundle "german" "from textbook" [card1, card2]
            let expected = "<table>" ++
                               "<caption>german (from textbook)</caption>" ++
                               (export . toHtml) card1 ++
                               (export . toHtml) card2 ++
                           "</table>"
            let actual = export $ toHtml bundle
            assertEqual "toHtml complex bundle with a description" expected actual
