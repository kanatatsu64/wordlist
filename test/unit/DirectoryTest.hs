{-# LANGUAGE BlockArguments #-}

module DirectoryTest (
    test_all,

    test_op,
    test_withExt,
    test_delExt,
    test_getName,
    test_getDir
) where

import Test.Tasty
import Test.Tasty.HUnit

import Directory ( (</>), withExt, delExt, getName, getDir )

test_all = testGroup "Directory" [
        test_op,
        test_withExt,
        test_delExt,
        test_getName,
        test_getDir
    ]

test_op = testGroup "(</>)" [
        test_op1,
        test_op2,
        test_op3,
        test_op4
    ]
    where
        test_op1 = testCase "(</>) 1" do
            let base = "base"
            let path = "path.ext"
            let expected = "base/path.ext"
            let actual = base </> path
            assertEqual "base without '/'" expected actual
        test_op2 = testCase "(</>) 2" do
            let base = "base/"
            let path = "path.ext"
            let expected = "base/path.ext"
            let actual = base </> path
            assertEqual "base with '/'" expected actual
        test_op3 = testCase "(</>) 3" do
            let base = "base"
            let path = "/path.ext"
            let expected = "/path.ext"
            let actual = base </> path
            assertEqual "absolute path 1" expected actual
        test_op4 = testCase "(</>) 4" do
            let base = "base/"
            let path = "/path.ext"
            let expected = "/path.ext"
            let actual = base </> path
            assertEqual "absolute path 2" expected actual

test_withExt = testGroup "withExt" [
        test_withExt1,
        test_withExt2,
        test_withExt3,
        test_withExt4,
        test_withExt5,
        test_withExt6,
        test_withExt7
    ]
    where
        test_withExt1 = testCase "withExt 1" do
            let ext = "ext"
            let path = "path.ext"
            let expected = True
            let actual = withExt ext path
            assertEqual "simple path" expected actual
        test_withExt2 = testCase "withExt 2" do
            let ext = "ext"
            let path = "path."
            let expected = False
            let actual = withExt ext path
            assertEqual "empty ext 1" expected actual
        test_withExt3 = testCase "withExt 3" do
            let ext = ""
            let path = "path."
            let expected = True
            let actual = withExt ext path
            assertEqual "empty ext 1" expected actual
        test_withExt4 = testCase "withExt 4" do
            let ext = ""
            let path = "dir"
            let expected = True
            let actual = withExt ext path
            assertEqual "directory" expected actual
        test_withExt5 = testCase "withExt 5" do
            let ext = "ext"
            let path = ".."
            let expected = False
            let actual = withExt ext path
            assertEqual "special directory 1" expected actual
        test_withExt6 = testCase "withExt 6" do
            let ext = ""
            let path = ".."
            let expected = True
            let actual = withExt ext path
            assertEqual "special directory 2" expected actual
        test_withExt7 = testCase "withExt 7" do
            let ext = "ext"
            let path = "dir/path.ext"
            let expected = True
            let actual = withExt ext path
            assertEqual "full path" expected actual

test_delExt = testGroup "delExt" [
        test_delExt1,
        test_delExt2,
        test_delExt3,
        test_delExt4,
        test_delExt5,
        test_delExt6
    ]
    where
        test_delExt1 = testCase "delExt 1" do
            let path = "path.ext"
            let expected = "path"
            let actual = delExt path
            assertEqual "simple path" expected actual
        test_delExt2 = testCase "delExt 2" do
            let path = "dir/path.ext"
            let expected = "dir/path"
            let actual = delExt path
            assertEqual "with directory" expected actual
        test_delExt3 = testCase "delExt 3" do
            let path = "../path.ext"
            let expected = "../path"
            let actual = delExt path
            assertEqual "with special directory" expected actual
        test_delExt4 = testCase "delExt 4" do
            let path = "path"
            let expected = "path"
            let actual = delExt path
            assertEqual "without extension 1" expected actual
        test_delExt5 = testCase "delExt 5" do
            let path = "dir/path"
            let expected = "dir/path"
            let actual = delExt path
            assertEqual "without extension 2" expected actual
        test_delExt6 = testCase "delExt 6" do
            let path = "../path"
            let expected = "../path"
            let actual = delExt path
            assertEqual "without extension 3" expected actual

test_getName = testGroup "getName" [
        test_getName1,
        test_getName2,
        test_getName3
    ]
    where
        test_getName1 = testCase "getName 1" do
            let path = "dir/path.ext"
            let expected = "path.ext"
            let actual = getName path
            assertEqual "simple path" expected actual
        test_getName2 = testCase "getName 2" do
            let path = "path.ext"
            let expected = "path.ext"
            let actual = getName path
            assertEqual "without directory" expected actual
        test_getName3 = testCase "getName 3" do
            let path = "dir/"
            let expected = ""
            let actual = getName path
            assertEqual "directory" expected actual

test_getDir = testGroup "getDir" [
        test_getDir1,
        test_getDir2,
        test_getDir3,
        test_getDir4
    ]
    where
        test_getDir1 = testCase "getDir 1" do
            let path = "dir/path.ext"
            let expected = "dir/"
            let actual = getDir path
            assertEqual "simple path" expected actual
        test_getDir2 = testCase "getDir 2" do
            let path = "path.ext"
            let expected = ""
            let actual = getDir path
            assertEqual "without directory" expected actual
        test_getDir3 = testCase "getDir 3" do
            let path = "dir/"
            let expected = "dir/"
            let actual = getDir path
            assertEqual "directory" expected actual
        test_getDir4 = testCase "getDir 4" do
            let path = "dir1/dir2/path"
            let expected = "dir1/dir2/"
            let actual = getDir path
            assertEqual "nested directory" expected actual
