module Server.Api.Card.ListTest (
    test_all,

    test_uploadHandler
) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai.Test

import Server.Json ( Json (..), Ary (..), Dict (..), Rec (..), jsonify )
import qualified Server.Card as Card
import qualified Plugins.German.Base as German
import TestUtils ( runAppTest, compile )
import Server.Api.Card.List ( uploadHandler )

test_all = testGroup "List" [
    ]

{- To DO -}
test_uploadHandler = undefined
