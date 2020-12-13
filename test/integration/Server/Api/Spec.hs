module Server.Api.Spec (
    test_all,

    test_bundle,
    test_card
) where

import Test.Tasty

import qualified Server.Api.Bundle.Spec as BundleTest
import qualified Server.Api.Card.Spec as CardTest

test_all = testGroup "Api" [
        test_bundle,
        test_card
    ]

test_bundle = BundleTest.test_all
test_card = CardTest.test_all
