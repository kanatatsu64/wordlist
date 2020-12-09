module Server.Internal.Spec (
    test_all,

    test_json,
    test_sql
) where

import Test.Tasty

import qualified Server.Internal.JsonTest as JsonTest
import qualified Server.Internal.SQLTest as SQLTest

test_all = testGroup "Internal" [
        test_json,
        test_sql
    ]

test_json = JsonTest.test_all
test_sql = SQLTest.test_all
