module Server.Spec (
    test_all,

    test_api
) where

import Test.Tasty

import qualified Server.Api.Spec as ApiTest

test_all = testGroup "Server" [
        test_api
    ]

test_api = ApiTest.test_all
