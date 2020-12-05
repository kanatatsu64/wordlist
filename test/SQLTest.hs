module SQLTest (
    test_all,

    test_buildSQL,
    test_nop,
    test_select,
    test_from,
    test_where_,
    test_insert,
    test_values,
    test_update,
    test_set,
    test_delete,
    test_createDatabase,
    test_createTable
) where

import Test.Tasty
import Test.Tasty.HUnit

import SQL (
        toSql,
        buildSQL,
        nop,
        select,
        from,
        where_,
        insert,
        values,
        update,
        set,
        delete,
        createDatabase,
        createTable
    )

test_all = testGroup "SQL" [
        test_buildSQL,
        test_nop,
        test_select,
        test_from,
        test_where_,
        test_insert,
        test_values,
        test_update,
        test_set,
        test_delete,
        test_createDatabase,
        test_createTable
    ]

test_buildSQL = testGroup "buildSQL" []

test_nop = testGroup "nop" [
        test_nop1,
        test_nop2
    ]
    where
        test_nop1 = testCase "nop 1" do
            let dsl = nop
                expected = return @Maybe ";"
                actual = buildSQL dsl
            assertEqual "nop once" expected actual
        test_nop2 = testCase "nop 2" do
            let dsl = do
                    nop
                    nop
                expected = return @Maybe ";"
                actual = buildSQL dsl
            assertEqual "nop twice" expected actual

test_select = testGroup "select" [
        test_select1,
        test_select2,
        test_select3
    ]
    where
        test_select1 = testCase "select 1" do
            let dsl = select ["col"]
                expected = return @Maybe "SELECT col ;"
                actual = buildSQL dsl
            assertEqual "single column" expected actual
        test_select2 = testCase "select 2" do
            let dsl = select ["col1", "col2", "col3"]
                expected = return @Maybe "SELECT col1,col2,col3 ;"
                actual = buildSQL dsl
            assertEqual "three column" expected actual
        test_select3 = testCase "select 3" do
            let dsl = select []
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "no column" expected actual

test_from = testGroup "from" [
        test_from1,
        test_from2
    ]
    where
        test_from1 = testCase "from 1" do
            let dsl = from "Table"
                expected = return @Maybe "FROM Table ;"
                actual = buildSQL dsl
            assertEqual "simple from" expected actual
        test_from2 = testCase "from 2" do
            let dsl = from ""
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "empty table name" expected actual

test_where_ = testGroup "where_" [
        test_where_1,
        test_where_2,
        test_where_3,
        test_where_4
    ]
    where
        test_where_1 = testCase "where_ 1" do
            let cond = ("id = ?", [toSql @Integer 100])
                dsl = where_ cond
                expected = return @Maybe "WHERE id = ? ;"
                actual = buildSQL dsl
            assertEqual "simple where" expected actual
        test_where_2 = testCase "where_ 2" do
            let cond = ("id = 100", [])
                dsl = where_ cond
                expected = return @Maybe "WHERE id = 100 ;"
                actual = buildSQL dsl
            assertEqual "no val" expected actual
        {- to fix -}
        test_where_3 = testCase "where_ 3" do
            let cond = ("id = 100", [toSql @Integer 100])
                dsl = where_ cond
                expected = return @Maybe "WHERE id = 100 ;"
                actual = buildSQL dsl
            assertEqual "too many vals" expected actual
        {- to fix -}
        test_where_4 = testCase "where_ 4" do
            let cond = ("id = ?", [])
                dsl = where_ cond
                expected = return @Maybe "WHERE id = ? ;"
                actual = buildSQL dsl
            assertEqual "too few vals" expected actual

test_insert = testGroup "insert" [
        test_insert1,
        test_insert2,
        test_insert3,
        test_insert4
    ]
    where
        test_insert1 = testCase "insert 1" do
            let dsl = insert "Table" ["col"]
                expected = return @Maybe "INSERT INTO Table (col) ;"
                actual = buildSQL dsl
            assertEqual "simple insert" expected actual
        test_insert2 = testCase "insert 2" do
            let dsl = insert "Table" ["col1", "col2"]
                expected = return @Maybe "INSERT INTO Table (col1,col2) ;"
                actual = buildSQL dsl
            assertEqual "two columns" expected actual
        test_insert3 = testCase "insert 3" do
            let dsl = insert "Table" []
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "no column" expected actual
        test_insert4 = testCase "insert 4" do
            let dsl = insert "" ["col"]
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "empty table name" expected actual

test_values = testGroup "values" [
        test_values1,
        test_values2,
        test_values3
    ]
    where
        test_values1 = testCase "values 1" do
            let dsl = values [toSql @Integer 1]
                expected = return @Maybe "VALUES (?) ;"
                actual = buildSQL dsl
            assertEqual "simple values" expected actual
        test_values2 = testCase "values 2" do
            let dsl = values [toSql @Integer 1, toSql "test", toSql True]
                expected = return @Maybe "VALUES (?,?,?) ;"
                actual = buildSQL dsl
            assertEqual "three values" expected actual
        test_values3 = testCase "values 3" do
            let dsl = values []
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "no value" expected actual

test_update = testGroup "update" [
        test_update1,
        test_update2
    ]
    where
        test_update1 = testCase "update 1" do
            let dsl = update "Table"
                expected = return @Maybe "UPDATE Table ;"
                actual = buildSQL dsl
            assertEqual "simple update" expected actual
        test_update2 = testCase "update 2" do
            let dsl = update ""
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "empty table name" expected actual

test_set = testGroup "set" [
        test_set1,
        test_set2,
        test_set3
    ]
    where
        test_set1 = testCase "set 1" do
            let records = [("col", toSql @Integer 1)]
                dsl = set records
                expected = return @Maybe "SET col = ? ;"
                actual = buildSQL dsl
            assertEqual "simple set" expected actual
        test_set2 = testCase "set 2" do
            let records = [
                        ("col1", toSql @Integer 10),
                        ("col2", toSql "test"),
                        ("col3", toSql True)
                    ]
                dsl = set records
                expected = return @Maybe "SET col1 = ?,col2 = ?,col3 = ? ;"
                actual = buildSQL dsl
            assertEqual "three records" expected actual
        test_set3 = testCase "set 3" do
            let records = []
                dsl = set records
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "no record" expected actual

test_delete = testGroup "delete" [
        test_delete1,
        test_delete2
    ]
    where
        test_delete1 = testCase "delete 1" do
            let dsl = delete "Table"
                expected = return @Maybe "DELETE FROM Table ;"
                actual = buildSQL dsl
            assertEqual "simple delete" expected actual
        test_delete2 = testCase "delete 2" do
            let dsl = delete ""
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "empty table name" expected actual

test_createDatabase = testGroup "createDatabase" [
        test_createDatabase1,
        test_createDatabase2
    ]
    where
        test_createDatabase1 = testCase "createDatabase 1" do
            let dsl = createDatabase "Database"
                expected = return @Maybe "CREATE DATABASE Database ;"
                actual = buildSQL dsl
            assertEqual "simple createDatabase" expected actual
        test_createDatabase2 = testCase "createDatabase 2" do
            let dsl = createDatabase ""
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "empty database name" expected actual

test_createTable = testGroup "createTable" [
        test_createTable1,
        test_createTable2,
        test_createTable3,
        test_createTable4
    ]
    where
        test_createTable1 = testCase "createTable 1" do
            let defs = [("col", "Int")]
                dsl = createTable "Table" defs
                expected = return @Maybe "CREATE TABLE Table (col Int) ;"
                actual = buildSQL dsl
            assertEqual "simple createTable" expected actual
        test_createTable2 = testCase "createTable 2" do
            let defs = [("col1", "Int"), ("col2", "Bool"), ("col3", "String")]
                dsl = createTable "Table" defs
                expected = return @Maybe "CREATE TABLE Table (col1 Int,col2 Bool,col3 String) ;"
                actual = buildSQL dsl
            assertEqual "three columns" expected actual
        test_createTable3 = testCase "createTable 3" do
            let defs = []
                dsl = createTable "Table" defs
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "no column" expected actual
        test_createTable4 = testCase "createTable 4" do
            let defs = [("col", "Int")]
                dsl = createTable "" defs
                expected = Nothing
                actual = buildSQL dsl
            assertEqual "empty table name" expected actual
