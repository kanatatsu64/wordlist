module German.Spec (
    test_all,

    test_noun,
    test_verb,
    test_adjective
) where

import Test.Tasty

import qualified German.NounTest as NounTest
import qualified German.VerbTest as VerbTest
import qualified German.AdjectiveTest as AdjectiveTest

test_all = testGroup "German" [
        test_noun,
        test_verb,
        test_adjective
    ]

test_noun = NounTest.test_all
test_verb = VerbTest.test_all
test_adjective = AdjectiveTest.test_all
