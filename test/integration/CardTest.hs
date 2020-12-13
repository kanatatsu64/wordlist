module CardTest (
    test_all,

    test_saveAndLoad
) where

import Test.Tasty
import Test.Tasty.HUnit

import Serial
import UUID
import Server.Card

test_all = testGroup "Card" [
        test_saveAndLoad
    ]

test_saveAndLoad = testCase "save and load" do
    _cardid <- getRandom
    _pluginid <- getRandom
    let _language = German
        _word = "klein"
        _meaning = "small"
        comp = Serial "-er"
        sup = Serial "-est"
        _attrs = [comp, sup]
        _note = "nothing special"
        _examples = [
                Example "ein kleines Haus" "a small hous",
                Example "zwei kleinen Kinder" "two small kids"
            ]
        card = Card _cardid _pluginid _language _word _meaning _attrs _note _examples
    save card
    card' <- load (cardid card)

    assertEqual "cardid" (cardid card) (cardid card')
    assertEqual "pluginid" (pluginid card) (pluginid card')
    assertEqual "word" (word card) (word card')
    assertEqual "meaning" (meaning card) (meaning card')
    assertAttrs (attrs card) (attrs card')
    assertEqual "note" (note card) (note card')
    assertExamples (examples card) (examples card')
    where
        assertAttrs [] [] = return ()
        assertAttrs (a:as) (b:bs) = do
            assertEqual "attrs" (serialize a) (serialize b)
            assertAttrs as bs
        assertAttrs _ _ = assertFailure "number of attrs is different"
        assertExamples [] [] = return ()
        assertExamples (Example o t:as) (Example o' t':bs) = do
            assertEqual "original" o o'
            assertEqual "translation" t t'
        assertExamples _ _ = assertFailure "number of examples is different"
