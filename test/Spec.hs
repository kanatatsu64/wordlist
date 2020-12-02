import Test.Tasty

import qualified CsvTest
import qualified HtmlTest
import qualified BundleTest
import qualified DirectoryTest
import qualified UtilsTest
import qualified Plugins.Spec as PluginsTest

test_all = testGroup "main" [
        test_csv,
        test_html,
        test_bundle,
        test_directory,
        test_utils,
        test_plugins
    ]

test_csv = CsvTest.test_all
test_html = HtmlTest.test_all
test_bundle = BundleTest.test_all
test_directory = DirectoryTest.test_all
test_utils = UtilsTest.test_all
test_plugins = PluginsTest.test_all

main :: IO ()
main = defaultMain test_all
