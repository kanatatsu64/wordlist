{-# LANGUAGE BlockArguments #-}

module Plugins.German.AdjectiveTest (
    test_all,

    test_parse,

    test_parseAttrs
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serial ( Serial (..), serialize )
import Plugins.German.Card ( GermanCard (..), Part (..), Example (..) )
import Plugins.German.Adjective (
        parse,

        parseAttrs
    )
import Utils ( maybeToFail )
import TestUtils ( getTestUUID )

test_all = testGroup "Adjective" [
        test_parse,
        test_parseAttrs
    ]

test_parse = testCase "parse" do
    let _word = "klein"
        _comp = "-er"
        _sup = "-est"
        _meaning = "small"
        _note = ""
        _original = "ein kleines Kind"
        _translation = "a small Child"
        vals = [
                _word,
                _comp,
                _sup,
                _meaning,
                _note,
                _original,
                _translation
            ]
    uuid <- getTestUUID
    card <- maybeToFail "failed to parse" $ parse vals (GermanCard uuid Adjective)
    assertEqual "word" _word (word card)

    let [comp', sup'] = map serialize (attrs card)
    assertEqual "comp" _comp comp'
    assertEqual "sup" _sup sup'

    assertEqual "meaning" _meaning (meaning card)
    assertEqual "note" _note (note card)

    let examples' = examples card
    assertEqual "length of examples" 1 (length examples')
    let [Example original' translation'] = examples'
    assertEqual "original" _original original'
    assertEqual "translation" _translation translation'

test_parseAttrs = testCase "parseAttrs" do
    let comp = "-er"
        sup = "-est"
        attr = [comp, sup, "dummy"]
        cons = id
        next = \vals [comp', sup'] -> do
            let actual = vals
                expected = ["dummy"]
            assertEqual "values are consumed" expected actual
            let actual = [serialize comp', serialize sup']
                expected = [comp, sup]
            assertEqual "arg to cons is right" expected actual
    f <- maybeToFail "failed to parseAttrs" $ parseAttrs attr cons
    f next
