{-# LANGUAGE BlockArguments #-}

module UtilsTest (
    test_all,

    test_split
) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils ( split )

test_all = testGroup "Utils" [
        test_split
    ]

test_split = testGroup "split" [
        test_split1,
        test_split2,
        test_split3,
        test_split4,
        test_split5
    ]
    where
        test_split1 = testCase "split 1" do
            let str = "a, b, c"
            let expected = ["a", " b", " c"]
            let actual = split ',' str
            assertEqual "split simple str" expected actual
        test_split2 = testCase "split 2" do
            let str = " , a  ,"
            let expected = [" ", " a  ", ""]
            let actual = split ',' str
            assertEqual "split str with empty parts" expected actual
        test_split3 = testCase "split 3" do
            let str = "a"
            let expected = ["a"]
            let actual = split ',' str
            assertEqual "split str without delimiter" expected actual
        test_split4 = testCase "split 4" do
            let str = ""
            let expected = [""]
            let actual = split ',' str
            assertEqual "split empty str" expected actual
        test_split5 = testCase "split 5" do
            let str = ","
            let expected = ["",""]
            let actual = split ',' str
            assertEqual "split single delimitor" expected actual
