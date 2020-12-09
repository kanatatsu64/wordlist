{-# LANGUAGE BlockArguments #-}

module Plugins.German.VerbTest (
    test_all,

    test_parse,

    test_isIntransitive,
    test_isTransitive,
    test_parseAttrs
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serial ( Serial (..), serialize )
import Plugins.German.Card ( GermanCard (..), Part (..), Example (..) )
import Plugins.German.Verb (
        parse,

        Kind (..),
        isIntransitive,
        isTransitive,
        parseAttrs
    )
import Utils ( maybeToFail )
import TestUtils ( getTestUUID )

test_all = testGroup "Verb" [
        test_parse,

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
            let _word = "sein"
                _kind = serialize $ Serial Intransitive
                _meaning = "is"
                _note = "illegal"
                _original = "Ich bin einen Mann."
                _translation = "I am a man."
                vals = [
                        _word,
                        _kind,
                        "",
                        _meaning,
                        _note,
                        _original,
                        _translation
                    ]
            uuid <- getTestUUID
            card <- maybeToFail "failed to parse" $ parse vals (GermanCard uuid Verb)
            assertEqual "word" _word (word card)

            let [kind'] = map serialize (attrs card)
            assertEqual "kind" (serialize $ Serial Intransitive) kind'

            assertEqual "meaning" _meaning (meaning card)
            assertEqual "note" _note (note card)

            let examples' = examples card
            assertEqual "length of examples" 1 (length examples')
            let [Example original' translation'] = examples'
            assertEqual "original" _original original'
            assertEqual "translation" _translation translation'
        test_parse2 = testCase "parse 2" do
            let _word = "haben"
                _kind = serialize $ Serial Transitive
                _form = "- (4)"
                _meaning = "have"
                _note = "illegal"
                _original = "Ich habe eines Haus."
                _translation = "I have a house."
                vals = [
                        _word,
                        _kind,
                        _form,
                        _meaning,
                        _note,
                        _original,
                        _translation
                    ]
            uuid <- getTestUUID
            card <- maybeToFail "failed to parse" $ parse vals (GermanCard uuid Verb)
            assertEqual "word" _word (word card)

            let [kind', form'] = map serialize (attrs card)
            assertEqual "kind" (serialize $ Serial Transitive) kind'
            assertEqual "form" _form form'

            assertEqual "meaning" _meaning (meaning card)
            assertEqual "note" _note (note card)

            let examples' = examples card
            assertEqual "length of examples" 1 (length examples')
            let [Example original' translation'] = examples'
            assertEqual "original" _original original'
            assertEqual "translation" _translation translation'

test_isIntransitive = testGroup "isIntransitive" [
        test_isIntransitive1,
        test_isIntransitive2
    ]
    where
        test_isIntransitive1 = testCase "isIntransitive 1" do
            let str = serialize $ Serial Intransitive
                actual = isIntransitive str
                expected = True
            assertEqual "isIntransitive Intransitive" expected actual
        test_isIntransitive2 = testCase "isIntransitive 2" do
            let str = serialize $ Serial Transitive
                actual = isIntransitive str
                expected = False
            assertEqual "isIntransitive Transitive" expected actual

test_isTransitive = testGroup "isTransitive" [
        test_isTransitive1,
        test_isTransitive2
    ]
    where
        test_isTransitive1 = testCase "isTransitive 1" do
            let str = serialize $ Serial Intransitive
                actual = isTransitive str
                expected = False
            assertEqual "isTransitive Intransitive" expected actual
        test_isTransitive2 = testCase "isTransitive 2" do
            let str = serialize $ Serial Transitive
                actual = isTransitive str
                expected = True
            assertEqual "isTransitive Transitive" expected actual

test_parseAttrs = testGroup "parseAttrs" [
        test_parseAttrs1,
        test_parseAttrs2
    ]
    where
        test_parseAttrs1 = testCase "parseAttrs 1" do
            let kind = serialize $ Serial Intransitive
                vals = [kind, "dummy1", "dummy2"]
                cons = id
                next = \vals' [kind'] -> do
                    let expected = ["dummy2"]
                        actual = vals'
                    assertEqual "values are consumed" expected actual
                    let expected = serialize $ Serial Intransitive
                        actual = serialize kind'
                    assertEqual "arg to cons is correct" expected actual
            f <- maybeToFail "failed to parseAttrs" $ parseAttrs vals cons
            f next
        test_parseAttrs2 = testCase "parseAttrs 2" do
            let kind = serialize $ Serial Transitive
                form = "- (4)"
                vals = [kind, form, "dummy"]
                cons = id
                next = \vals' [kind', form'] -> do
                    let expected = ["dummy"]
                        actual = vals'
                    assertEqual "values are consumed" expected actual
                    let expected = serialize $ Serial Transitive
                        actual = serialize kind'
                    assertEqual "kind" expected actual
                    let expected = "- (4)"
                        actual = serialize form'
                    assertEqual "form" expected actual
            f <- maybeToFail "failed to parseAttrs" $ parseAttrs vals cons
            f next
