{-# LANGUAGE BlockArguments #-}

module Plugins.German.NounTest (
    test_all,

    test_parse,

    test_isMale,
    test_isFemale,
    test_isNeuter,
    test_parseAttrs
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serial ( Serial (..), serialize )
import Plugins.German.Card ( GermanCard (..), Part (..), Example (..) )
import Plugins.German.Noun (
        parse,

        Genre (..),
        isMale,
        isFemale,
        isNeuter,
        parseAttrs
    )
import Utils ( maybeToFail )
import TestUtils ( getTestUUID )

test_all = testGroup "Noun" [
        test_parse,

        test_isMale,
        test_isFemale,
        test_isNeuter,
        test_parseAttrs
    ]

test_parse = testCase "parse" do
    let _word = "Name"
        _pl = "-n"
        _gen = serialize $ Serial Male
        _meaning = "name"
        _note = "illegal"
        _original = "mein Name"
        _translation = "my name"
        vals = [
                _word,
                _pl,
                _gen,
                _meaning,
                _note,
                _original,
                _translation
            ]
    uuid <- getTestUUID
    card <- maybeToFail "failed to parse" $ parse vals (GermanCard uuid Noun)
    assertEqual "word" _word (word card)

    let [pl', gen'] = map serialize (attrs card)
    assertEqual "pl" _pl pl'
    assertEqual "gen" _gen gen'

    assertEqual "meaning" _meaning (meaning card)
    assertEqual "note" _note (note card)

    let examples' = examples card
    assertEqual "length of examples" 1 (length examples')
    let [Example original' translation'] = examples'
    assertEqual "original" _original original'
    assertEqual "translation" _translation translation'

test_isMale = testGroup "isMale" [
        test_isMale1,
        test_isMale2,
        test_isMale3
    ]
    where
        test_isMale1 = testCase "isMale 1" do
            let str = serialize $ Serial Male
                actual = isMale str
                expected = True
            assertEqual "isMale Male" expected actual
        test_isMale2 = testCase "isMale 2" do
            let str = serialize $ Serial Female
                actual = isMale str
                expected = False
            assertEqual "isMale Female" expected actual
        test_isMale3 = testCase "isMale 3" do
            let str = serialize $ Serial Neuter
                actual = isMale str
                expected = False
            assertEqual "isMale Neuter" expected actual

test_isFemale = testGroup "isFemale" [
        test_isFemale1,
        test_isFemale2,
        test_isFemale3
    ]
    where
        test_isFemale1 = testCase "isFemale 1" do
            let str = serialize $ Serial Male
                actual = isFemale str
                expected = False
            assertEqual "isFemale Male" expected actual
        test_isFemale2 = testCase "isFemale 2" do
            let str = serialize $ Serial Female
                actual = isFemale str
                expected = True
            assertEqual "isFemale Female" expected actual
        test_isFemale3 = testCase "isFemale 3" do
            let str = serialize $ Serial Neuter
                actual = isFemale str
                expected = False
            assertEqual "isFemale Neuter" expected actual

test_isNeuter = testGroup "isNeuter" [
        test_isNeuter1,
        test_isNeuter2,
        test_isNeuter3
    ]
    where
        test_isNeuter1 = testCase "isNeuter 1" do
            let str = serialize $ Serial Male
                actual = isNeuter str
                expected = False
            assertEqual "isNeuter Male" expected actual
        test_isNeuter2 = testCase "isNeuter 2" do
            let str = serialize $ Serial Female
                actual = isNeuter str
                expected = False
            assertEqual "isNeuter Female" expected actual
        test_isNeuter3 = testCase "isNeuter 3" do
            let str = serialize $ Serial Neuter
                actual = isNeuter str
                expected = True
            assertEqual "isNeuter Neuter" expected actual

test_parseAttrs = testCase "parseAttrs" do
    let pl = Serial "plural"
        gen = Serial Male
        attr = [serialize pl, serialize gen, "dummy"]
        cons = id
        next = \vals [pl', gen'] -> do
            let actual = vals
                expected = ["dummy"]
            assertEqual "values are consumed" expected actual
            let actual = [serialize pl', serialize gen']
                expected = [serialize pl, serialize gen]
            assertEqual "arg to cons is right" expected actual
    f <- maybeToFail "failed to parseAttrs" $ parseAttrs attr cons
    f next
