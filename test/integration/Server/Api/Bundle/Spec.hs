module Server.Api.Bundle.Spec (
    test_all,

    test_bundle,
    test_cards,
    test_list
) where

import Test.Tasty

import qualified Server.Api.Bundle.BundleTest as BundleTest
import qualified Server.Api.Bundle.CardsTest as CardsTest
import qualified Server.Api.Bundle.ListTest as ListTest

test_all = testGroup "Bundle" [
        test_bundle,
        test_cards,
        test_list
    ]

test_bundle = BundleTest.test_all
test_cards = CardsTest.test_all
test_list = ListTest.test_all
