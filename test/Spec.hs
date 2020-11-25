import Test.Tasty

import qualified HtmlTest
import qualified German.Spec as GermanTest

test_all = testGroup "main" [
        test_html,
        GermanTest.test_all
    ]

test_html = testGroup "Html" [
        HtmlTest.test_export,
        HtmlTest.test_exportHtml,
        HtmlTest.test_exportAttr,
        HtmlTest.test_exportContent,
        HtmlTest.test_cons
    ]

main :: IO ()
main = defaultMain test_all
