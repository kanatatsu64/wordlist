{-# LANGUAGE BlockArguments #-}

module HtmlTest (
    test_export,
    test_exportHtml,
    test_exportAttr,
    test_exportContent,
    test_cons
) where

import Test.Tasty
import Test.Tasty.HUnit

import Html

test_export = testCase "export" do
    let attrs1 = ["attr1-1", "attr1-2"]
    let contents1 = [
                Text "content1",
                Text "content2"
            ]
    let html1 = Tag TR attrs1 contents1
    let attr2 = ["attr2-1", "attr2-2", "attr2-3"]
    let contents2 = [
                Child html1,
                Text "content3"
            ]
    let html2 = Tag TD attr2 contents2
    let expected = "<td attr2-1 attr2-2 attr2-3>" ++
                   "<tr attr1-1 attr1-2>content1content2</tr>" ++
                   "content3" ++
                   "</td>"
    let actual = export html2
    let msg = "export complex html"
    assertEqual msg expected actual


test_exportHtml = testGroup "exportHtml" [
        test_exportHtml1,
        test_exportHtml2,
        test_exportHtml3
    ]
    where
        test_exportHtml1 = testCase "exportHtml 1" do
            let attrs = ["attr1", "attr2"]
            let contents = [
                        Text "content1",
                        Text "content2"
                    ]
            let html = Tag TD attrs contents
            let expected = "<td attr1 attr2>content1content2</td>"
            let actual = exportHtml html
            let msg = "exportHtml simple html"
            assertEqual msg expected actual

        test_exportHtml2 = testCase "exportHtml 2" do
            let attrs1 = ["attr1-1", "attr1-2"]
            let contents1 = [
                        Text "content1",
                        Text "content2"
                    ]
            let html1 = Tag TR attrs1 contents1
            let attr2 = ["attr2-1", "attr2-2", "attr2-3"]
            let contents2 = [
                        Child html1,
                        Child html1
                    ]
            let html2 = Tag TD attr2 contents2
            let expected = "<td attr2-1 attr2-2 attr2-3>" ++
                           "<tr attr1-1 attr1-2>content1content2</tr>" ++
                           "<tr attr1-1 attr1-2>content1content2</tr>" ++
                           "</td>"
            let actual = exportHtml html2
            let msg = "exportHtml complex html"
            assertEqual msg expected actual
        test_exportHtml3 = testCase "exportHtml 3" do
            let html = Tag TD [] []
            let expected = "<td></td>"
            let actual = exportHtml html
            let msg = "exportHtml empty tag"
            assertEqual msg expected actual

test_exportAttr = testCase "exportAttr" do
    let attr = "attr"
    let expected = "attr"
    let actual = exportAttr attr
    let msg = "exportAttr string"
    assertEqual msg expected actual

test_exportContent = testGroup "exportContent" [
        test_exportContent1,
        test_exportContent2
    ]
    where
        test_exportContent1 = testCase "exportContent 1" do
            let content = Text "test content"
            let expected = "test content"
            let actual = exportContent content
            let msg = "export text content"
            assertEqual msg expected actual

        test_exportContent2 = testCase "exportContent 2" do
            let html = Tag TD ["attr1", "attr2"] [
                                Text "inner"
                            ]
            let content = Child html
            let expected = "<td attr1 attr2>inner</td>"
            let actual = exportContent content
            let msg = "export html content"
            assertEqual msg expected actual

test_cons = testGroup "cons" [
        test_cons1,
        test_cons2,
        test_cons3
    ]
    where
        test_cons1 = testCase "cons 1" do
            let delim = " "
            let actual = cons delim ["a", "b", "c"]
            let expected = "a b c"
            let msg = "compose whitespace-separated string"
            assertEqual msg expected actual

        test_cons2 = testCase "cons 2" do
            let delim = ":"
            let actual = cons delim ["a", "b", "c"]
            let expected = "a:b:c"
            let msg = "compose colon-separated string"
            assertEqual msg expected actual

        test_cons3 = testCase "cons 3" do
            let delim = ""
            let actual = cons delim ["a", "b", "c"]
            let expected = "abc"
            let msg = "compose joined string"
            assertEqual msg expected actual
