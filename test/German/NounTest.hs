{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module German.NounTest (
    test_parse,
    test_toHtml,

    test_isMale,
    test_isFemale,
    test_isNeuter,
    test_parseAttrs,
    test_consPl
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serializable ( Serializable (..) )
import Html ( export, TagName (..), Html (..), Content (..) )
import German.Card ( Card, Part (..), Attr (..), Example (..) )
import qualified German.Card as Card ( Card (..) )
import German.Noun (
        parse,
        toHtml,

        Genre (..),
        isMale,
        isFemale,
        isNeuter,
        parseAttrs,
        consPl
    )

test_parse = testCase "parse" do
    let word = "Name"
    let pl = "-n"
    let gen = serialize Male
    let meaning = "name"
    let note = "illegal"
    let original = "mein Name"
    let translation = "my name"
    let vals = [
                word,
                pl,
                gen,
                meaning,
                note,
                original,
                translation
            ]
    let card :: Card = parse vals (Card.Card "cardid" Noun)
    assertEqual "word" word (Card.word card)

    let [pl', gen'] = Card.attrs card
    assertEqual "pl" (serialize pl) (serialize pl')
    assertEqual "gen" gen (serialize gen')

    assertEqual "meaning" meaning (Card.meaning card)
    assertEqual "note" note (Card.note card)

    let examples' = Card.examples card
    assertEqual "length of examples" 1 (length examples')
    let [Example original' translation'] = examples'
    assertEqual "original" original original'
    assertEqual "translation" translation translation'

test_toHtml = testCase "toHtml" do
    let word = "Name"
    let pl = Attr "-n"
    let gen = Attr Male
    let attrs = [pl, gen]
    let meaning = "name"
    let note = "illegal"
    let original = "mein Name"
    let translation = "my name"
    let examples = [Example original translation]
    let card = Card.Card "cardid" Noun word attrs meaning note examples
    let expected = Tag TR [] [
                Child $ Tag TD [] [Text "Name - Namen"],
                Child $ Tag TD [] [Text "M. name (illegal)"]
            ]
    let actual = toHtml card
    assertEqual "html string" (export expected) (export actual)

test_isMale = testGroup "isMale" [
        test_isMale1,
        test_isMale2,
        test_isMale3
    ]
    where
        test_isMale1 = testCase "isMale1" do
            let str = serialize Male
            let actual = isMale str
            let expected = True
            assertEqual "isMale Male" expected actual
        test_isMale2 = testCase "isMale2" do
            let str = serialize Female
            let actual = isMale str
            let expected = False
            assertEqual "isMale Female" expected actual
        test_isMale3 = testCase "isMale3" do
            let str = serialize Neuter
            let actual = isMale str
            let expected = False
            assertEqual "isMale Neuter" expected actual

test_isFemale = testGroup "isFemale" [
        test_isFemale1,
        test_isFemale2,
        test_isFemale3
    ]
    where
        test_isFemale1 = testCase "isFemale1" do
            let str = serialize Male
            let actual = isFemale str
            let expected = False
            assertEqual "isFemale Male" expected actual
        test_isFemale2 = testCase "isFemale2" do
            let str = serialize Female
            let actual = isFemale str
            let expected = True
            assertEqual "isFemale Female" expected actual
        test_isFemale3 = testCase "isFemale3" do
            let str = serialize Neuter
            let actual = isFemale str
            let expected = False
            assertEqual "isFemale Neuter" expected actual

test_isNeuter = testGroup "isNeuter" [
        test_isNeuter1,
        test_isNeuter2,
        test_isNeuter3
    ]
    where
        test_isNeuter1 = testCase "isNeuter1" do
            let str = serialize Male
            let actual = isNeuter str
            let expected = False
            assertEqual "isNeuter Male" expected actual
        test_isNeuter2 = testCase "isNeuter2" do
            let str = serialize Female
            let actual = isNeuter str
            let expected = False
            assertEqual "isNeuter Female" expected actual
        test_isNeuter3 = testCase "isNeuter3" do
            let str = serialize Neuter
            let actual = isNeuter str
            let expected = True
            assertEqual "isNeuter Neuter" expected actual

test_parseAttrs = testCase "parseAttrs" do
    let pl = "plural"
    let gen = Male
    let attr = [pl, serialize gen, "dummy"]
    let cons = id
    let next = \vals [pl', gen'] -> do
            let actual = vals
            let expected = ["dummy"]
            assertEqual "values are consumed" expected actual
            let actual = [serialize pl', serialize gen']
            let expected = [serialize pl, serialize gen]
            assertEqual "arg to cons is right" expected actual
    parseAttrs attr cons next

test_consPl = testGroup "consPl" [
        test_consPl1,
        test_consPl2
    ]
    where
        test_consPl1 = testCase "consPl 1" do
            let pl = "-er"
            let word = "Kind"
            let actual = consPl pl word
            let expected = "Kinder"
            assertEqual "consPl interpolation" expected actual
        test_consPl2 = testCase "consPl 2" do
            let pl = "Häuser"
            let word = "Haus"
            let actual = consPl pl word
            let expected = "Häuser"
            assertEqual "consPl illegular" expected actual
