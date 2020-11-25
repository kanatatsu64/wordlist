module German.Spec (
    test_all,
    test_noun,
    test_verb
) where

import Test.Tasty

import qualified German.NounTest as NounTest

test_all = testGroup "German" [
        test_noun
    ]

test_noun = testGroup "German/Noun" [
        NounTest.test_parse,
        NounTest.test_toHtml,

        NounTest.test_isMale,
        NounTest.test_isFemale,
        NounTest.test_isNeuter,
        NounTest.test_parseAttrs,
        NounTest.test_consPl
    ]

test_verb = testGroup "German/Verb" [

    ]
