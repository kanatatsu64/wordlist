module Server.Api.Card.Spec (
    test_all,

    test_card,
    test_list
) where

import Test.Tasty

import qualified Server.Api.Card.CardTest as CardTest
import qualified Server.Api.Card.ListTest as ListTest

test_all = testGroup "Card" [
        test_card,
        test_list
    ]

test_card = CardTest.test_all
test_list = ListTest.test_all
