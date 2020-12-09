{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Internal.JsonTest (
    test_all,

    test_parse,
    test_jsonify,

    test_parseInt,
    test_parseFloat,
    test_parseString,
    test_parseSeq,
    test_parseJInt,
    test_parseJFloat,
    test_parseJString,
    test_parseJAry,
    test_parseJDict,
    test_parseJObj
) where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ( lookup )
import qualified Text.Parsec as Parsec
import Text.Parsec ( space, noneOf, anyToken, many )
import Data.Sort ( sortOn )

import Convertible ( Convertible (..), ConvertError (..) )
import Server.Json ( lookup, Ary (..), Dict (..), Rec (..) )
import Server.Internal.Json

test_all = testGroup "Json" [
        test_parse,
        test_jsonify,

        test_parseInt,
        test_parseFloat,
        test_parseString,
        test_parseSeq,
        test_parseJInt,
        test_parseJFloat,
        test_parseJString,
        test_parseJAry,
        test_parseJDict,
        test_parseJObj
    ]

instance Eq JObj where
    (JInt a) == (JInt b) = a == b
    (JFloat a) == (JFloat b) = a == b
    (JString a) == (JString b) = a == b
    (JAry as) == (JAry bs) = as == bs
    (JDict as) == (JDict bs) =
        sortOn genKey as == sortOn genKey bs
        where genKey (JRec key _) = key
    _ == _ = False

instance Eq JRec where
    (JRec ak ao) == (JRec bk bo) = ak == bk && ao == bo

instance Eq Json where
    (Json a) == (Json b) = a == b

instance Show Json where
    show (Json str) = str

data Person = Person { name :: String, age :: Int }
    deriving (Eq, Show)

instance Convertible Person JObj where
    safeConvert (Person name age) = safeConvert jobject
        where jobject = Dict [
                    Rec "name" name,
                    Rec "age" age
                ]

instance Convertible JObj Person where
    safeConvert jobj@(JDict recs) = do
        _name <- lookup "name" recs error
        _age <- lookup "age" recs error
        Right $ Person _name _age
        where
            sourceValue = show jobj
            sourceType = "JDict"
            destType = "Person"
            message = "failed to parse JDict as Person"
            error = ConvertError sourceValue sourceType destType message
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "Person"
            message = "failed to parse JObj as Person"
            error = ConvertError sourceValue sourceType destType message

test_parse = testGroup "parse" [
        test_parse1,
        test_parse2,
        test_parse3,
        test_parse4,
        test_parse5,
        test_parse6,
        test_parse7,
        test_parse8
    ]
    where
        test_parse1 = testCase "parse 1" do
            let str = " 1 "
                json = Json str
                expected = 1
                msg = "parse Int"
            case parse @Int json of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parse2 = testCase "parse 2" do
            let str = " .1 "
                json = Json str
                expected = 0.1
                msg = "parse Float"
            case parse @Float json of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parse3 = testCase "parse 3" do
            let str = " \"test\" "
                json = Json str
                expected = "test"
                msg = "parse String"
            case parse @String json of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parse4 = testCase "parse 4" do
            let str = " [1, 2, 3] "
                json = Json str
                expected = [1, 2, 3]
                msg = "parse array"
            case parse @(Ary Int) json of
                Right (Ary actual) -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parse5 = testCase "parse 5" do
            let str = "{\n" ++
                      "\t\"name\": \"John\",\n" ++
                      "\t\"age\": 20\n" ++
                      "}\n"
                json = Json str
                expected = Person "John" 20
                msg = "parse object"
            case parse @Person json of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parse6 = testCase "parse 6" do
            let str = " .1 "
                json = Json str
                msg = "parse Float as Int"
            case parse @Int json of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parse7 = testCase "parse 7" do
            let str = " \"1\" "
                json = Json str
                expected = 1
                msg = "parse String as Int"
            case parse @Int json of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parse8 = testCase "parse 8" do
            let str = "{\n" ++
                      "\t\"name\": \"John\",\n" ++
                      "\t\"age\": 20.0\n" ++
                      "}\n"
                json = Json str
                msg = "parse invalid object"
            case parse @Person json of
                Right _ -> assertFailure msg
                Left _ -> return ()

test_jsonify = testGroup "jsonify" [
        test_jsonify1,
        test_jsonify2,
        test_jsonify3,
        test_jsonify4,
        test_jsonify5,
        test_jsonify6
    ]
    where
        test_jsonify1 = testCase "jsonify 1" do
            let val :: Int = 1
                expected = Json "1"
                actual = jsonify val
            assertEqual "jsonify Int" expected actual 
        test_jsonify2 = testCase "jsonify 2" do
            let val :: Float = 1.0
                expected = Json "1.0"
                actual = jsonify val
            assertEqual "jsonify Float" expected actual 
        test_jsonify3 = testCase "jsonify 3" do
            let val = "test"
                expected = Json "\"test\""
                actual = jsonify val
            assertEqual "jsonify String" expected actual 
        test_jsonify4 = testCase "jsonify 4" do
            let val :: Ary Int = Ary [1, 2, 3]
                expected = Json "[1,2,3]"
                actual = jsonify val
            assertEqual "jsonify Ary" expected actual 
        test_jsonify5 = testCase "jsonify 5" do
            let uid :: Int = 10
                home = "/home/John"
                val = Dict [
                        Rec "uid" uid,
                        Rec "home" home
                    ]
                expected = Json "{\"uid\":10,\"home\":\"/home/John\"}"
                actual = jsonify val
            assertEqual "jsonify Dict" expected actual 
        test_jsonify6 = testCase "jsonify 6" do
            let val = Person "John" 20
                expected = Json "{\"name\":\"John\",\"age\":20}"
                actual = jsonify val
            assertEqual "jsonify object" expected actual 

test_parseInt = testGroup "parseInt" [
        test_parseInt1,
        test_parseInt2,
        test_parseInt3,
        test_parseInt4,
        test_parseInt5,
        test_parseInt6,
        test_parseInt7,
        test_parseInt8,
        test_parseInt9,
        test_parseInt10,
        test_parseInt11
    ]
    where
        parse = Parsec.parse parseInt ""
        test_parseInt1 = testCase "parseInt 1" do
            let input = "100"
                expected = 100
                msg = "parseInt simple Int"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt2 = testCase "parseInt 2" do
            let input = "100_"
                expected = 100
                msg = "parseInt Int followed by _"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt3 = testCase "parseInt 3" do
            let input = "_100"
                msg = "parseInt Int following _"
            case parse input of 
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseInt4 = testCase "parseInt 4" do
            let input = "010"
                msg = "parseInt Int starting with zero"
            case parse input of 
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseInt5 = testCase "parseInt 5" do
            let input = "0"
                expected = 0
                msg = "parseInt zero"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt6 = testCase "parseInt 6" do
            let input = "+1"
                expected = 1
                msg = "parseInt Int with +"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt7 = testCase "parseInt 7" do
            let input = "-1"
                expected = -1
                msg = "parseInt Int with -"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt8 = testCase "parseInt 8" do
            let input = "+0"
                expected = 0
                msg = "parseInt zero with +"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt9 = testCase "parseInt 9" do
            let input = "-0"
                expected = 0
                msg = "parseInt zero with -"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseInt10 = testCase "parseInt 10" do
            let input = "+-1"
                msg = "parseInt Int with +-"
            case parse input of 
                Right _ -> assertFailure msg
                Left _ -> return ()
        {- note -}
        test_parseInt11 = testCase "parseInt 11" do
            let input = "0.1"
                expected = 0
                msg = "parseInt Float containing ."
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseFloat = testGroup "parseFloat" [
        test_parseFloat1,
        test_parseFloat2,
        test_parseFloat3,
        test_parseFloat4,
        test_parseFloat5,
        test_parseFloat6,
        test_parseFloat7,
        test_parseFloat8,
        test_parseFloat9,
        test_parseFloat10,
        test_parseFloat11,
        test_parseFloat12,
        test_parseFloat13,
        test_parseFloat14,
        test_parseFloat15,
        test_parseFloat16,
        test_parseFloat17,
        test_parseFloat18,
        test_parseFloat19,
        test_parseFloat20
    ]
    where
        parse = Parsec.parse parseFloat ""
        test_parseFloat1 = testCase "parseFloat 1" do
            let input = "0.1"
                expected = 0.1
                msg = "parseFloat simple Float"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat2 = testCase "parseFloat 2" do
            let input = ".1"
                expected = 0.1
                msg = "parseFloat Float starting with point"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat3 = testCase "parseFloat 3" do
            let input = "+1.0"
                expected = 1
                msg = "parseFloat Float with +"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat4 = testCase "parseFloat 4" do
            let input = "-1.0"
                expected = -1
                msg = "parseFloat Float with -"
            case parse input of 
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat5 = testCase "parseFloat 5" do
            let input = "+-1.0"
                msg = "parseFloat Float with +-"
            case parse input of 
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat6 = testCase "parseFloat 6" do
            let input = "1"
                msg = "parseFloat Int"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat7 = testCase "parseFloat 7" do
            let input = "0"
                msg = "parseFloat zero"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat8 = testCase "parseFloat 8" do
            let input = "0.1_"
                expected = 0.1
                msg = "parseFloat Float followed by _"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat9 = testCase "parseFloat 9" do
            let input = "_0.1"
                msg = "parseFloat Float following _"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat10 = testCase "parseFloat 10" do
            let input = ".00"
                expected = 0
                msg = "parseFloat zero starting with point"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat11 = testCase "parseFloat 11" do
            let input = "0.10"
                expected = 0.1
                msg = "parseFloat Float followed by additional zero"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat12 = testCase "parseFloat 12" do
            let input = "01"
                msg = "parseFloat Int following additional zero"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat13 = testCase "parseFloat 13" do
            let input = "00.1"
                msg = "parseFloat Float following additional zero"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat14 = testCase "parseFloat 14" do
            let input = "1."
                expected = 1
                msg = "parseFloat Float ending with point"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat15 = testCase "parseFloat 15" do
            let input = "0."
                expected = 0
                msg = "parseFloat zero ending with point"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseFloat16 = testCase "parseFloat 16" do
            let input = "."
                msg = "parseFloat point"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat17 = testCase "parseFloat 17" do
            let input = "1.."
                msg = "parseFloat Float ending with two points"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat18 = testCase "parseFloat 18" do
            let input = "..1"
                msg = "parseFloat Float starting with two point"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat19 = testCase "parseFloat 19" do
            let input = "0.."
                msg = "parseFloat zero ending with two points"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseFloat20 = testCase "parseFloat 20" do
            let input = "..0"
                msg = "parseFloat zero starting with two point"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()

test_parseString = testGroup "parseString" [
        test_parseString1,
        test_parseString2,
        test_parseString3,
        test_parseString4,
        test_parseString5,
        test_parseString6,
        test_parseString7,
        test_parseString8
    ]
    where
        parse = Parsec.parse parseString ""
        test_parseString1 = testCase "parseString 1" do
            let input = "\"test\""
                expected = "test"
                msg = "parseString simple String"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseString2 = testCase "parseString 2" do
            let input = "_\"test\""
                msg = "parseString String following _"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseString3 = testCase "parseString 3" do
            let input = "\"test\"_"
                expected = "test"
                msg = "parseString String followed by _"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseString4 = testCase "parseString 4" do
            let input = "te\"st"
                msg = "parseString String with single quatation"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseString5 = testCase "parseString 5" do
            let input = "test"
                msg = "parseString String without quatation"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseString6 = testCase "parseString 6" do
            let input = "\"test\\\"\""
                expected = "test\\\""
                msg = "parseString String with escaped quatation"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseString7 = testCase "parseString 7" do
            let input = "\"test\\n\""
                expected = "test\\n"
                msg = "parseString String with escaped newline"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        {- note -}
        test_parseString8 = testCase "parseString 8" do
            let input = "\"test\"\""
                expected = "test"
                msg = "parseString String ending with quatation"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseSeq = testGroup "parseSeq" [
        test_parseSeq1,
        test_parseSeq2,
        test_parseSeq3,
        test_parseSeq4,
        test_parseSeq5,
        test_parseSeq6
    ]
    where
        parse = Parsec.parse (parseSeq parseInt) ""
        parseStop = Parsec.parse (parseSeq (many (noneOf ","))) ""
        parseThrough = Parsec.parse (parseSeq (many anyToken)) ""
        test_parseSeq1 = testCase "parseSeq 1" do
            let input = "1,2"
                expected = [1, 2]
                msg = "parseSeq simple seq"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseSeq2 = testCase "parseSeq 2" do
            let input = "1,2,"
                expected = ["1", "2", ""]
                msg = "parseSeq seq ending with ,"
            case parseStop input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseSeq3 = testCase "parseSeq 3" do
            let input = "1"
                expected = [1]
                msg = "parseSeq seq without separator"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseSeq4 = testCase "parseSeq 4" do
            let input = ""
                expected = []
                msg = "parseSeq empty"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseSeq5 = testCase "parseSeq 5" do
            let input = "1,02"
                msg = "parseSeq seq containing invalid value"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        {- note -}
        test_parseSeq6 = testCase "parseSeq 6" do
            let input = "1,2"
                expected = ["1,2"]
                msg = "parseSeq with greedy parser"
            case parseThrough input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseJInt = testGroup "parseJInt" [
        test_parseJInt1,
        test_parseJInt2,
        test_parseJInt3,
        test_parseJInt4
    ]
    where
        parse = Parsec.parse parseJInt ""
        test_parseJInt1 = testCase "parseJInt 1" do
            let input = "1"
                expected = JInt 1
                msg = "parseJInt simple Int"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJInt2 = testCase "parseJInt 2" do
            let input = "0"
                expected = JInt 0
                msg = "parseJInt zero"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        {- note -}
        test_parseJInt3 = testCase "parseJInt 3" do
            let input = "1.0"
                expected = JInt 1
                msg = "parseJInt simple Float"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJInt4 = testCase "parseJInt 4" do
            let input = " \n  1  "
                expected = JInt 1
                msg = "parseJInt Int with spaces"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseJFloat = testGroup "parseJFloat" [
        test_parseJFloat1,
        test_parseJFloat2,
        test_parseJFloat3,
        test_parseJFloat4,
        test_parseJFloat5,
        test_parseJFloat6
    ]
    where
        parse = Parsec.parse parseJFloat ""
        test_parseJFloat1 = testCase "parseJFloat 1" do
            let input = "1.0"
                expected = JFloat 1
                msg = "parseJFloat simple Float"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJFloat2 = testCase "parseJFloat 2" do
            let input = "1."
                expected = JFloat 1
                msg = "parseJFloat Float ending with dot"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJFloat3 = testCase "parseJFloat 3" do
            let input = ".1"
                expected = JFloat 0.1
                msg = "parseJFloat Float starting with dot"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJFloat4 = testCase "parseJFloat 4" do
            let input = "1"
                msg = "parseJFloat simple Int"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJFloat5 = testCase "parseJFloat 5" do
            let input = "0"
                msg = "parseJFloat zero"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJFloat6 = testCase "parseJFloat 6" do
            let input = " \n  1.0  "
                expected = JFloat 1
                msg = "parseJFloat Float with spaces"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseJString = testGroup "parseJString" [
        test_parseJString1,
        test_parseJString2,
        test_parseJString3
    ]
    where
        parse = Parsec.parse parseJString ""
        test_parseJString1 = testCase "parseJString 1" do
            let input = "\"test\""
                expected = JString "test"
                msg = "parseJString simple String"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJString2 = testCase "parseJString 2" do
            let input = "1"
                msg = "parseJString simple Int"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJString3 = testCase "parseJString 3" do
            let input = " \t \"te st\n\"  "
                expected = JString "te st\n"
                msg = "parseJString String with spaces"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseJAry = testGroup "parseJAry" [
        test_parseJAry1,
        test_parseJAry2,
        test_parseJAry3,
        test_parseJAry4,
        test_parseJAry5,
        test_parseJAry6,
        test_parseJAry7,
        test_parseJAry8,
        test_parseJAry9,
        test_parseJAry10,
        test_parseJAry11,
        test_parseJAry12,
        test_parseJAry13
    ]
    where
        parse = Parsec.parse parseJAry ""
        test_parseJAry1 = testCase "parseJAry 1" do
            let input = "[\"test1\",\"test2\"]"
                expected = JAry [
                        JString "test1",
                        JString "test2"
                    ]
                msg = "parseJAry String array"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry2 = testCase "parseJAry 2" do
            let input = "[1,2]"
                expected = JAry [
                        JInt 1,
                        JInt 2
                    ]
                msg = "parseJAry Int array"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry3 = testCase "parseJAry 3" do
            let input = "[1.0,2.0]"
                expected = JAry [
                        JFloat 1.0,
                        JFloat 2.0
                    ]
                msg = "parseJAry Float array"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry4 = testCase "parseJAry 4" do
            let input = "[\"test\"]"
                expected = JAry [ JString "test" ]
                msg = "parseJAry Array with signel element"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry5 = testCase "parseJAry 5" do
            let input = "[1,.2]"
                expected = JAry [
                        JInt 1,
                        JFloat 0.2
                    ]
                msg = "parseJAry mixed array 1"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry6 = testCase "parseJAry 6" do
            let input = "[1.,\"test\"]"
                expected = JAry [
                        JFloat 1,
                        JString "test"
                    ]
                msg = "parseJAry mixed array 2"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry7 = testCase "parseJAry 7" do
            let input = "[[.0,\"test\"]]"
                expected = JAry [
                        JAry [
                                JFloat 0,
                                JString "test"
                            ]
                    ]
                msg = "parseJAry nested array"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry8 = testCase "parseJAry 8" do
            let input = "[1.,[2,\"test\"]]"
                expected = JAry [
                        JFloat 1,
                        JAry [
                                JInt 2,
                                JString "test"
                            ]
                    ]
                msg = "parseJAry partially nested array"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry9 = testCase "parseJAry 9" do
            let input = "1.,2,\"test\"]"
                msg = "parseJAry array without opening bracket 1"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        {- note -}
        test_parseJAry10 = testCase "parseJAry 10" do
            let input = "[1.,2,\"test\"]]"
                expected = JAry [
                        JFloat 1,
                        JInt 2,
                        JString "test"
                    ]
                msg = "parseJAry array without opening bracket 2"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJAry11 = testCase "parseJAry 11" do
            let input = "[1.,2,\"test\""
                msg = "parseJAry array without closing bracket 1"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJAry12 = testCase "parseJAry 12" do
            let input = "[1.,2,[\"test\"]"
                msg = "parseJAry array without closing bracket 2"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJAry13 = testCase "parseJAry 13" do
            let input = "  [  1.  ,\t  2  ,  \n" ++
                        " [  \"te st\"  ] \n ]  "
                expected = JAry [
                        JFloat 1,
                        JInt 2,
                        JAry [
                                JString "te st"
                            ]
                    ]
                msg = "parseJAry array with spaces"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseJDict = testGroup "parseJDict" [
        test_parseJDict1,
        test_parseJDict2,
        test_parseJDict3,
        test_parseJDict4,
        test_parseJDict5,
        test_parseJDict6,
        test_parseJDict7,
        test_parseJDict8,
        test_parseJDict9,
        test_parseJDict10,
        test_parseJDict11,
        test_parseJDict12,
        test_parseJDict13,
        test_parseJDict14
    ]
    where
        parse = Parsec.parse parseJDict ""
        test_parseJDict1 = testCase "parseJDict 1" do
            let input = "{\"key\":\"value\"}"
                expected = JDict [
                        JRec "key" (JString "value")
                    ]
                msg = "parseJDict simple Dict"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJDict2 = testCase "parseJDict 2" do
            let input = "{\"key1\":\"test\",\"key2\":1}"
                expected = JDict [
                        JRec "key1" (JString "test"),
                        JRec "key2" (JInt 1)
                    ]
                msg = "parseJDict Dict with two elements"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJDict3 = testCase "parseJDict 3" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "{\"key21\":1,\"key22\":.2}" ++
                        "}"
                expected = JDict [
                        JRec "key1" (JString "test"),
                        JRec "key2" (JDict [
                                JRec "key21" (JInt 1),
                                JRec "key22" (JFloat 0.2)
                            ])
                    ]
                msg = "parseJDict Dict with two elements"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJDict4 = testCase "parseJDict 4" do
            let input = "\"key1\":\"test\",\"key2\":1}"
                msg = "parseJDict Dict without opening bracket 1"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict5 = testCase "parseJDict 5" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "\"key21\":1,\"key22\":.2}" ++
                        "}"
                msg = "parseJDict Dict without opening bracket 2"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict6 = testCase "parseJDict 6" do
            let input = "{\"key1\":\"test\",\"key2\":1"
                msg = "parseJDict Dict without closing bracket 1"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict7 = testCase "parseJDict 7" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "{\"key21\":1,\"key22\":.2" ++
                        "}"
                msg = "parseJDict Dict without closing bracket 2"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict8 = testCase "parseJDict 8" do
            let input = "{\"key1\"\"test\",\"key2\":1}"
                msg = "parseJDict Dict without : 1"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict9 = testCase "parseJDict 9" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "{\"key21\"1,\"key22\":.2}" ++
                        "}"
                msg = "parseJDict Dict without : 2"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict10 = testCase "parseJDict 10" do
            let input = "{1:\"test\",\"key2\":1}"
                msg = "parseJDict Dict with invalid key 1"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict11 = testCase "parseJDict 11" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "{21:1,\"key22\":.2}" ++
                        "}"
                msg = "parseJDict Dict with invalid key 2"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict12 = testCase "parseJDict 12" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "{\"key21:1,\"key22\":.2}" ++
                        "}"
                msg = "parseJDict Dict with invalid key 3"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict13 = testCase "parseJDict 13" do
            let input = "{\"key1\":\"test\",\"key2\":" ++
                        "{key21\":1,\"key22\":.2}" ++
                        "}"
                msg = "parseJDict Dict with invalid key 4"
            case parse input of
                Right _ -> assertFailure msg
                Left _ -> return ()
        test_parseJDict14 = testCase "parseJDict 14" do
            let input = "  {  \"key1\"\t  :  \"te  st\"  ,  \"key2\"  :  \n" ++
                        "  {  \"key 21\"  :\t1, \n"++
                        " \"key22\"  :  .2}  " ++
                        "   }"
                expected = JDict [
                        JRec "key1" (JString "te  st"),
                        JRec "key2" (JDict [
                                JRec "key 21" (JInt 1),
                                JRec "key22" (JFloat 0.2)
                            ])
                    ]
                msg = "parseJDict Dict with spaces"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg

test_parseJObj = testGroup "parseJObj" [
        test_parseJObj1,
        test_parseJObj2,
        test_parseJObj3,
        test_parseJObj4,
        test_parseJObj5,
        test_parseJObj6,
        test_parseJObj7
    ]
    where
        parse = Parsec.parse parseJObj ""
        test_parseJObj1 = testCase "parseJObj 1" do
            let input = "1"
                expected = JInt 1
                msg = "parseJObj simple Int"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJObj2 = testCase "parseJObj 2" do
            let input = "1.0"
                expected = JFloat 1
                msg = "parseJObj simple Float"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJObj3 = testCase "parseJObj 3" do
            let input = "[1,[2.,\"test\"]]"
                expected = JAry [
                        JInt 1,
                        JAry [
                                JFloat 2,
                                JString "test"
                            ]
                    ]
                msg = "parseJObj array"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJObj4 = testCase "parseJObj 4" do
            let input = "{\"key1\":1,\"key2\":{\"key21\":.2,\"key22\":\"test\"}}"
                expected = JDict [
                        JRec "key1" (JInt 1),
                        JRec "key2" (JDict [
                                JRec "key21" (JFloat 0.2),
                                JRec "key22" (JString "test")
                            ])
                    ]
                msg = "parseJObj Dict"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJObj5 = testCase "parseJObj 5" do
            let input = "{\"key1\":0,\"key2\":[1.0,\"test\"]}"
                expected = JDict [
                        JRec "key1" (JInt 0),
                        JRec "key2" (JAry [
                                JFloat 1,
                                JString "test"
                            ])
                    ]
                msg = "parseJObj complex Obj 1"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJObj6 = testCase "parseJObj 6" do
            let input = "[.0,{\"key1\":0.,\"key2\":\"test\"}]"
                expected = JAry [
                        JFloat 0,
                        JDict [
                                JRec "key1" (JFloat 0),
                                JRec "key2" (JString "test")
                            ]
                    ]
                msg = "parseJObj complex Obj 2"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
        test_parseJObj7 = testCase "parseJObj 7" do
            let input = "[.0,\n{ \"key1\":  0., \n \"key2\": \t  \"test\"}\n] \n"
                expected = JAry [
                        JFloat 0,
                        JDict [
                                JRec "key1" (JFloat 0),
                                JRec "key2" (JString "test")
                            ]
                    ]
                msg = "parseJObj Obj with spaces"
            case parse input of
                Right actual -> assertEqual msg expected actual
                Left _ -> assertFailure msg
