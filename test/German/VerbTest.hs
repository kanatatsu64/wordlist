{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module German.VerbTest (
    test_all,

    test_parse,
    test_toHtml,

    test_isIntransitive,
    test_isTransitive,
    test_parseAttrs
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serializable ( Serializable (..) )
import Html ( export, TagName (..), Html (..), Content (..) )
import German.Card ( Card, Part (..), Attr (..), Example (..) )
import qualified German.Card as Card ( Card (..) )
import German.Verb (
        parse,
        toHtml,

        Kind (..),
        isIntransitive,
        isTransitive,
        parseAttrs
    )

test_all = testGroup "German/Verb" [
        test_parse,
        test_toHtml,

        test_isIntransitive,
        test_isTransitive,
        test_parseAttrs
    ]

test_parse = testGroup "parse" [
        test_parse1,
        test_parse2
    ]
    where
        test_parse1 = testCase "parse 1" do
            let word = "sein"
            let kind = serialize Intransitive
            let meaning = "is"
            let note = "illegal"
            let original = "Ich bin einen Mann."
            let translation = "I am a man."
            let vals = [
                        word,
                        kind,
                        "",
                        meaning,
                        note,
                        original,
                        translation
                    ]
            let card :: Card = parse vals (Card.Card "cardid" Verb)
            assertEqual "word" word (Card.word card)

            let [kind'] = Card.attrs card
            assertEqual "kind" (serialize Intransitive) (serialize kind')

            assertEqual "meaning" meaning (Card.meaning card)
            assertEqual "note" note (Card.note card)

            let examples' = Card.examples card
            assertEqual "length of examples" 1 (length examples')
            let [Example original' translation'] = examples'
            assertEqual "original" original original'
            assertEqual "translation" translation translation'
        test_parse2 = testCase "parse 2" do
            let word = "haben"
            let kind = serialize Transitive
            let form = "- (4)"
            let meaning = "have"
            let note = "illegal"
            let original = "Ich habe eines Haus."
            let translation = "I have a house."
            let vals = [
                        word,
                        kind,
                        form,
                        meaning,
                        note,
                        original,
                        translation
                    ]
            let card :: Card = parse vals (Card.Card "cardid" Verb)
            assertEqual "word" word (Card.word card)

            let [kind', form'] = Card.attrs card
            assertEqual "kind" (serialize Transitive) (serialize kind')
            assertEqual "form" form (serialize form')

            assertEqual "meaning" meaning (Card.meaning card)
            assertEqual "note" note (Card.note card)

            let examples' = Card.examples card
            assertEqual "length of examples" 1 (length examples')
            let [Example original' translation'] = examples'
            assertEqual "original" original original'
            assertEqual "translation" translation translation'

test_toHtml = testGroup "toHtml" [
        test_toHtml1,
        test_toHtml2
    ]
    where
        test_toHtml1 = testCase "toHtml 1" do
            let word = "sein"
            let kind = Attr Intransitive
            let attrs = [kind]
            let meaning = "is"
            let note = "illegal"
            let original = "Ich bin einen Mann."
            let translation = "I am a man."
            let examples = [Example original translation]
            let card = Card.Card "cardid" Noun word attrs meaning note examples
            let expected = Tag TR [] [
                        Child $ Tag TD [] [Text "sein"],
                        Child $ Tag TD [] [Text "I. is (illegal)"]
                    ]
            let actual = toHtml card
            assertEqual "html string" (export expected) (export actual)
        test_toHtml2 = testCase "toHtml 2" do
            let word = "haben"
            let kind = Attr Transitive
            let form = Attr "- (4)"
            let attrs = [kind, form]
            let meaning = "have"
            let note = "illegal"
            let original = "Ich habe eines Haus."
            let translation = "I have a house."
            let examples = [Example original translation]
            let card = Card.Card "cardid" Noun word attrs meaning note examples
            let expected = Tag TR [] [
                        Child $ Tag TD [] [Text "haben"],
                        Child $ Tag TD [] [Text "T. have (illegal)"]
                    ]
            let actual = toHtml card
            assertEqual "html string" (export expected) (export actual)

test_isIntransitive = testGroup "isIntransitive" [
        test_isIntransitive1,
        test_isIntransitive2
    ]
    where
        test_isIntransitive1 = testCase "isIntransitive 1" do
            let str = serialize Intransitive
            let actual = isIntransitive str
            let expected = True
            assertEqual "isIntransitive Intransitive" expected actual
        test_isIntransitive2 = testCase "isIntransitive 2" do
            let str = serialize Transitive
            let actual = isIntransitive str
            let expected = False
            assertEqual "isIntransitive Transitive" expected actual

test_isTransitive = testGroup "isTransitive" [
        test_isTransitive1,
        test_isTransitive2
    ]
    where
        test_isTransitive1 = testCase "isTransitive 1" do
            let str = serialize Intransitive
            let actual = isTransitive str
            let expected = False
            assertEqual "isTransitive Intransitive" expected actual
        test_isTransitive2 = testCase "isTransitive 2" do
            let str = serialize Transitive
            let actual = isTransitive str
            let expected = True
            assertEqual "isTransitive Transitive" expected actual

test_parseAttrs = testGroup "parseAttrs" [
        test_parseAttrs1,
        test_parseAttrs2
    ]
    where
        test_parseAttrs1 = testCase "parseAttrs 1" do
            let kind = serialize Intransitive
            let vals = [kind, "dummy1", "dummy2"]
            let cons = id
            let next = \vals' [kind'] -> do
                    let expected = ["dummy2"]
                    let actual = vals'
                    assertEqual "values are consumed" expected actual
                    let expected = serialize Intransitive
                    let actual = serialize kind'
                    assertEqual "arg to cons is correct" expected actual
            parseAttrs vals cons next
        test_parseAttrs2 = testCase "parseAttrs 2" do
            let kind = serialize Transitive
            let form = "- (4)"
            let vals = [kind, form, "dummy"]
            let cons = id
            let next = \vals' [kind', form'] -> do
                    let expected = ["dummy"]
                    let actual = vals'
                    assertEqual "values are consumed" expected actual
                    let expected = serialize Transitive
                    let actual = serialize kind'
                    assertEqual "kind" expected actual
                    let expected = "- (4)"
                    let actual = serialize form'
                    assertEqual "form" expected actual
            parseAttrs vals cons next
