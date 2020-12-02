module Plugins.Spec (
    test_all,

    test_german
) where

import Test.Tasty

import qualified Plugins.German.Spec as GermanTest

test_all = testGroup "Plugins" [
        test_german
    ]

test_german = GermanTest.test_all
