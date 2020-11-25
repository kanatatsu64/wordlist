{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module German.AdjectiveTest (
    test_all,

    test_parse,
    test_toHtml,

    test_parseAttrs
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serializable ( Serializable (..) )
import Html ( export, TagName (..), Html (..), Content (..) )
import German.Card ( Card, Part (..), Attr (..), Example (..) )
import qualified German.Card as Card ( Card (..) )
import German.Adjective (
        parse,
        toHtml,

        Comparative (..),
        Superlative (..),
        parseAttrs
    )

test_all = testGroup "German/Adjective" [
        test_parse,
        test_toHtml,
        test_parseAttrs
    ]

test_parse = testCase "parse" do
    let word = "klein"
    let comp = "-er"
    let sup = "-est"
    let meaning = "small"
    let note = ""
    let original = "ein kleines Kind"
    let translation = "a small Child"
    let vals = [
                word,
                comp,
                sup,
                meaning,
                note,
                original,
                translation
            ]
    let card :: Card = parse vals (Card.Card "cardid" Adjective)
    assertEqual "word" word (Card.word card)

    let [comp', sup'] = Card.attrs card
    assertEqual "comp" comp (serialize comp')
    assertEqual "sup" sup (serialize sup')

    assertEqual "meaning" meaning (Card.meaning card)
    assertEqual "note" note (Card.note card)

    let examples' = Card.examples card
    assertEqual "length of examples" 1 (length examples')
    let [Example original' translation'] = examples'
    assertEqual "original" original original'
    assertEqual "translation" translation translation'

test_toHtml = testCase "toHtml" do
    let word = "klein"
    let comp = Attr $ Comparative "-er"
    let sup = Attr $ Superlative "-est"
    let attrs = [comp, sup]
    let meaning = "small"
    let note = ""
    let original = "ein kleines Kind"
    let translation = "a small child"
    let examples = [Example original translation]
    let card = Card.Card "cardid" Adjective word attrs meaning note examples
    let expected = Tag TR [] [
                Child $ Tag TD [] [Text "klein"],
                Child $ Tag TD [] [Text "Adj. small"]
            ]
    let actual = toHtml card
    assertEqual "html string" (export expected) (export actual)

test_parseAttrs = testCase "parseAttrs" do
    let comp = "-er"
    let sup = "-est"
    let attr = [comp, sup, "dummy"]
    let cons = id
    let next = \vals [comp', sup'] -> do
            let actual = vals
            let expected = ["dummy"]
            assertEqual "values are consumed" expected actual
            let actual = [serialize comp', serialize sup']
            let expected = [comp, sup]
            assertEqual "arg to cons is right" expected actual
    parseAttrs attr cons next
