{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module German.AdverbTest (
    test_all,

    test_toHtml
) where

import Test.Tasty
import Test.Tasty.HUnit

import Html ( export, TagName (..), Html (..), Content (..) )
import German.Card ( Part (..) )
import qualified German.Card as Card ( Card (..) )
import German.Adverb (
        toHtml
    )

test_all = testGroup "German/Adverb" [
        test_toHtml
    ]

test_toHtml = testCase "toHtml" do
    let word = "schon"
    let attrs = []
    let meaning = "already"
    let note = ""
    let examples = []
    let card = Card.Card "cardid" Adverb word attrs meaning note examples
    let expected = Tag TR [] [
                Child $ Tag TD [] [Text "schon"],
                Child $ Tag TD [] [Text "Adv. already"]
            ]
    let actual = toHtml card
    assertEqual "html string" (export expected) (export actual)
