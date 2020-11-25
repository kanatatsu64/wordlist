{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module German.ConjunctionTest (
    test_all,

    test_toHtml
) where

import Test.Tasty
import Test.Tasty.HUnit

import Html ( export, TagName (..), Html (..), Content (..) )
import German.Card ( Part (..) )
import qualified German.Card as Card ( Card (..) )
import German.Conjunction (
        toHtml
    )

test_all = testGroup "German/Conjunction" [
        test_toHtml
    ]

test_toHtml = testCase "toHtml" do
    let word = "und"
    let attrs = []
    let meaning = "and"
    let note = ""
    let examples = []
    let card = Card.Card "cardid" Conjunction word attrs meaning note examples
    let expected = Tag TR [] [
                Child $ Tag TD [] [Text "und"],
                Child $ Tag TD [] [Text "Kon. and"]
            ]
    let actual = toHtml card
    assertEqual "html string" (export expected) (export actual)
