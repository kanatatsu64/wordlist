module Integration.Spec (
    test_all,
    initTest,

    test_card
) where

import Test.Tasty

import Integration.TestInit ( initTest )
import qualified Integration.CardTest as CardTest

test_all = testGroup "Integration" [
        test_card
    ]

test_card = CardTest.test_all
