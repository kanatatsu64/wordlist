import Test.Tasty

import qualified CsvTest
import qualified HtmlTest
import qualified German.Spec as GermanTest

test_all = testGroup "main" [
        test_csv,
        test_html,
        test_german
    ]

test_csv = CsvTest.test_all
test_html = HtmlTest.test_all
test_german = GermanTest.test_all

main :: IO ()
main = defaultMain test_all
