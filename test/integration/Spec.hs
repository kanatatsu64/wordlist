import Test.Tasty

import TestInit ( initTest )
import qualified Server.Spec as ServerTest
import qualified CardTest as CardTest

test_all = testGroup "Integration Tests" [
        test_server,
        test_card
    ]

test_server = ServerTest.test_all
test_card = CardTest.test_all

main :: IO ()
main = do
    initTest
    defaultMain test_all
