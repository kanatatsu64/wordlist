import Test.Tasty

import TestInit ( initTest )
import qualified CardTest as CardTest

test_all = testGroup "Integration Tests" [
        test_card
    ]

test_card = CardTest.test_all

main :: IO ()
main = do
    initTest
    defaultMain test_all
