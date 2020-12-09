module Server.Spec (
    test_all,

    test_internal
) where

import Test.Tasty

import qualified Server.Internal.Spec as InternalTest

test_all = testGroup "Server" [
        test_internal
    ]

test_internal = InternalTest.test_all
