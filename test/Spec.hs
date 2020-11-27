import Test.Tasty

import qualified CsvTest
import qualified HtmlTest
import qualified BundleTest
import qualified DirectoryTest
import qualified UtilsTest
import qualified German.Spec as GermanTest

test_all = testGroup "main" [
        test_csv,
        test_html,
        test_bundle,
        test_directory,
        test_utils,
        test_german
    ]

test_csv = CsvTest.test_all
test_html = HtmlTest.test_all
test_bundle = BundleTest.test_all
test_directory = DirectoryTest.test_all
test_utils = UtilsTest.test_all
test_german = GermanTest.test_all

main :: IO ()
main = defaultMain test_all
