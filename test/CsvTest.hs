{-# LANGUAGE BlockArguments #-}

module CsvTest (
    test_all,

    test_parseRow,
    test_split,
    test_trim
) where

import Test.Tasty
import Test.Tasty.HUnit

import Csv ( parseRow, split, trim )

test_all = testGroup "Csv" [
        test_parseRow,
        test_split,
        test_trim
    ]

test_parseRow = testGroup "parseRow" [
        test_parseRow1,
        test_parseRow2,
        test_parseRow3,
        test_parseRow4
    ]
    where
        test_parseRow1 = testCase "parseRow 1" do
            let str = "a,b,c"
            let expected = ["a", "b", "c"]
            let actual = parseRow str
            assertEqual "parseRow simple str" expected actual
        test_parseRow2 = testCase "parseRow 2" do
            let str = " a b , c  d , e "
            let expected = ["a b", "c  d", "e"]
            let actual = parseRow str
            assertEqual "parseRow str with spaces" expected actual
        test_parseRow3 = testCase "parseRow 3" do
            let str = " , a ,,  , "
            let expected = ["", "a", "", "", ""]
            let actual = parseRow str
            assertEqual "parseRow str with empty columns 1" expected actual
        test_parseRow4 = testCase "parseRow 4" do
            let str = ", a ,,"
            let expected = ["", "a", "", ""]
            let actual = parseRow str
            assertEqual "parseRow str with empty columns 2" expected actual

test_split = testGroup "split" [
        test_split1,
        test_split2
    ]
    where
        test_split1 = testCase "split 1" do
            let str = "a, b, c"
            let expected = ["a", " b", " c"]
            let actual = split str
            assertEqual "split simple str" expected actual
        test_split2 = testCase "split 2" do
            let str = " , a  ,"
            let expected = [" ", " a  ", ""]
            let actual = split str
            assertEqual "split str with empty parts" expected actual

test_trim = testGroup "trim" [
        test_trim1,
        test_trim2,
        test_trim3,
        test_trim4
    ]
    where
        test_trim1 = testCase "trim 1" do
            let str = "abc"
            let expected = "abc"
            let actual = trim str
            assertEqual "identical trim" expected actual
        test_trim2 = testCase "trim 2" do
            let str = "abc  "
            let expected = "abc"
            let actual = trim str
            assertEqual "trim str with spaces at last" expected actual
        test_trim3 = testCase "trim 3" do
            let str = "  abc"
            let expected = "abc"
            let actual = trim str
            assertEqual "trim str with spaces at the beginning" expected actual
        test_trim4 = testCase "trim 4" do
            let str = "  abc "
            let expected = "abc"
            let actual = trim str
            assertEqual "trim str with spaces at the both sides" expected actual
