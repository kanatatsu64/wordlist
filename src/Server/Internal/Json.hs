{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Internal.Json (
    JKey,
    JRec (..),
    JObj (..),
    Json (..),
    parse,
    jsonify,
    safeJsonify,

    parseInt,
    parseFloat,
    parseString,
    parseSeq,
    parseJInt,
    parseJFloat,
    parseJString,
    parseJAry,
    parseJDict,
    parseJObj
) where

import Control.Monad
import Text.Read ( readMaybe )
import Text.Parsec ()
import Text.Parsec hiding ( parse )
import qualified Text.Parsec ( parse )
import Text.ParserCombinators.Parsec ( CharParser )

import Convertible ( Convertible (..), ConvertError (..), ConvertResult (..), convert )

type JKey = String
data JRec = JRec JKey JObj
data JObj = JInt Int | JFloat Float | JString String | JAry [JObj] | JDict [JRec]

newtype Json = Json String

parse :: Convertible JObj a => Json -> ConvertResult a
parse json = safeConvert json >>= safeConvert @JObj

safeJsonify :: Convertible a JObj => a -> ConvertResult Json
safeJsonify obj = safeConvert obj >>= safeConvert @JObj

jsonify :: Convertible a JObj => a -> Json
jsonify = convert @JObj . convert

instance Convertible String JObj where
    safeConvert = Right . JString

instance Convertible JObj String where
    safeConvert (JString str) = Right str
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "String"
            message = "failed to parse jobj as String"
            error = ConvertError sourceValue sourceType destType message

instance Convertible Int JObj where
    safeConvert = Right . JInt

instance Convertible JObj Int where
    safeConvert (JInt int) = Right int
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "Int"
            message = "failed to parse JObj as Int"
            error = ConvertError sourceValue sourceType destType message

instance Convertible Float JObj where
    safeConvert = Right . JFloat

instance Convertible JObj Float where
    safeConvert (JFloat float) = Right float
    safeConvert jobj = Left error
        where
            sourceValue = show jobj
            sourceType = "JObj"
            destType = "Float"
            message = "failed to parse JObj as Float"
            error = ConvertError sourceValue sourceType destType message

instance Show JObj where
    show jobj = case safeConvert jobj of
        Right (Json str) -> str
        Left _ -> "failed to parse JObj"

instance Convertible JObj Json where
    safeConvert (JInt int) = Right . Json . show $ int
    safeConvert (JFloat float) = Right . Json . show $ float
    safeConvert (JString str) = Right . Json $ "\"" ++ str ++ "\""
    safeConvert (JAry jobjs) = do
        rs <- mapM safeConvert jobjs
        Right . Json $ "[" ++ loop rs ++ "]"
        where
            loop [] = ""
            loop [Json str] = str
            loop (Json str:rs) = str ++ "," ++ loop rs
    safeConvert (JDict jrecs) = do
        cs <- mapM convertRec jrecs
        Right . Json $ "{" ++ loop cs ++ "}"
        where
            loop [] = ""
            loop [c] = c
            loop (c:cs) = c ++ "," ++ loop cs
            convertRec (JRec jkey jobj) = do
                (Json str) <- safeConvert jobj
                (Json key) <- safeConvert (JString jkey)
                Right $ key ++ ":" ++ str

instance Convertible Json JObj where
    safeConvert (Json str) = case _parse "" str of
        Right jobj -> Right jobj
        Left error -> Left $ getError error
        where
            sourceValue = str
            sourceType = "RawJson"
            destType = "JObj"
            getError = ConvertError sourceValue sourceType destType

_parse :: SourceName -> [Char] -> Either String JObj
_parse name input = case Text.Parsec.parse _parseJObj name input of
    Right json -> Right json
    Left error -> Left $ show error
    where
        _parseJObj = do
            jobj <- parseJObj
            spaces
            eof
            return jobj

(<||>) :: [a] -> [a] -> [a]
[] <||> bs = bs
as <||> _ = as

tryMaybe :: CharParser st a -> CharParser st (Maybe a)
tryMaybe = optionMaybe . try

choiceTry :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choiceTry ps = choice (map try ps)

notTests :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m Bool
notTests ps = lookAhead test
    where test = choice (map (\p -> try p >> return False) ps) <|> return True

parseInt :: CharParser st Int
parseInt = do
    ms <- tryMaybe $ oneOf "+-"
    res <- choiceTry [zero, withDigit]
    str <- case ms of
        Just '-' -> return ('-':res)
        Just '+' -> return res
        Nothing -> return res
    case readMaybe str of
        Just int -> return int
        Nothing -> fail $ "failed to parse as Int: " ++ str
    where
        zero = do
            char '0'
            cond <- notTests [digit]
            if cond
            then return "0"
            else fail "non-zero Int must begin with non-zero"
        withDigit = do
            h <- digit
            when (h == '0') $ fail "non-zero Int must begin with non-zero."
            res <- many digit
            return (h:res)

parseFloat :: CharParser st Float
parseFloat = do
    ms <- tryMaybe $ oneOf "+-"
    res <- choiceTry [withZero, withDot, withDigit]
    str <- case ms of
        Just '-' -> return ('-':res)
        Just '+' -> return res
        Nothing -> return res
    case readMaybe str of
        Just float -> return float
        Nothing -> fail $ "failed to parse as Float: " ++ str
    where
        withZero = do
            char '0'
            char '.'
            res <- many digit
            cond <- notTests [char '.']
            if cond
            then return ("0." ++ (res<||>"0"))
            else fail "unexpected '.'"
        withDot = do
            char '.'
            res <- many1 digit
            return ("0." ++ res)
        withDigit = do 
            h <- digit
            when (h == '0') $ fail "invalid Float"
            res1 <- many digit
            char '.'
            res2 <- many digit
            cond <- notTests [char '.']
            if cond
            then return ((h:res1) ++ "." ++ (res2<||>"0"))
            else fail "unexpected '.'"

parseString :: CharParser st String
parseString = between (char '\"') (char '\"') do
    strs <- many(try withEscape <|> (return <$> noneOf "\""))
    return $ join strs
    where
        withEscape = do
            char '\\'
            c <- anyChar
            return ['\\', c]

parseSeq :: CharParser st a -> CharParser st [a]
parseSeq p = sepBy _parse (char ',')
    where
        _parse = do
            res <- p
            spaces
            return res

parseJInt :: CharParser st JObj
parseJInt = do
    spaces
    JInt <$> parseInt

parseJFloat :: CharParser st JObj
parseJFloat = do
    spaces
    JFloat <$> parseFloat

parseJString :: CharParser st JObj
parseJString = do
    spaces
    JString <$> parseString

parseJAry :: CharParser st JObj
parseJAry = do
    spaces
    JAry <$> p
    where p = between (char '[') (char ']') (parseSeq parseJObj)

parseJDict :: CharParser st JObj
parseJDict = do
    spaces
    JDict <$> p
    where
        p = between (char '{') (char '}') (parseSeq parseRec)
        parseRec = do
            spaces
            key <- parseString
            spaces
            char ':'
            jobj <- parseJObj
            return $ JRec key jobj

parseJObj :: CharParser st JObj
parseJObj = do
    spaces
    choiceTry [parseJString, parseJFloat, parseJInt, parseJAry, parseJDict]
