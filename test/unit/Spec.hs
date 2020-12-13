import Test.Tasty

import qualified CsvTest
import qualified HtmlTest
import qualified BundleTest
import qualified DirectoryTest
import qualified UtilsTest
import qualified Plugins.Spec as PluginsTest
import qualified Server.Spec as ServerTest

test_all = testGroup "Unit Tests" [
        test_csv,
        test_html,
        test_bundle,
        test_directory,
        test_utils,
        test_plugins,
        test_server
    ]

test_csv = CsvTest.test_all
test_html = HtmlTest.test_all
test_bundle = BundleTest.test_all
test_directory = DirectoryTest.test_all
test_utils = UtilsTest.test_all
test_plugins = PluginsTest.test_all
test_server = ServerTest.test_all

main :: IO ()
main = defaultMain test_all
