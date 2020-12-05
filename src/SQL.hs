{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module SQL (
    database,
    connect,
    withConnection,
    contConnection,
    contTransaction,
    existTable,
    maybeFromSql,
    Runtime,
    execRuntime,
    runSQL,
    runQuery,
    SQL,
    Database,
    Table,
    Column,
    Condition,
    DataType,
    Definition,
    Record,
    ISchema (..),
    SQLDSL,
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
    createTable,
    runSelect,
    runInsert,
    runUpdate,
    runDelete,
    module Database.HDBC
) where

import Control.Monad.Cont
import Control.Monad.Writer
import Database.HDBC
import Data.Convertible ( Convertible )
import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )

import Utils ( execCont )

database :: Database
database = "resource/database"

connect :: FilePath -> IO Connection
connect = connectSqlite3

withConnection :: FilePath ->
                  (Connection -> IO a) ->
                  IO a
withConnection path callback = do
    connection <- connect path
    result <- callback connection
    disconnect connection
    return result

contConnection path = cont $ withConnection path

contTransaction conn = cont $ withTransaction conn

existTable :: IConnection conn => conn -> String -> IO Bool
existTable conn name = elem name <$> getTables conn

maybeFromSql :: Convertible SqlValue a => SqlValue -> Maybe a
maybeFromSql sval = case safeFromSql sval of
                      Left _ -> Nothing
                      Right val -> val

type Runtime c a = c -> IO a

execRuntime :: (forall c. IConnection c => Runtime c a) -> IO a
execRuntime runtime = execCont do
    conn <- contConnection database
    trans <- contTransaction conn
    return $ runtime trans

runSQL :: IConnection conn => SQL -> [SqlValue] -> Runtime conn Integer
runSQL sql vals conn = run conn sql vals

runQuery :: IConnection conn => SQL -> [SqlValue] -> [Column] -> Runtime conn [[Record]]
runQuery sql vals cols conn = do
    rows <- quickQuery conn sql vals
    return $ zip cols <$> rows

type SQL = String
type Database = String
type Table = String
type Column = String
type Condition = (SQL, [SqlValue])
type DataType = String
type Definition = (Column, DataType)

type Record = (Column, SqlValue)

class ISchema a where
    columns :: a -> [Column]
    toRecords :: a -> [Record]
    fromRecords :: [Record] -> Maybe a

type SQLDSL m = WriterT SQL m ()

buildSQL :: MonadFail m => SQLDSL m -> m SQL
buildSQL dsl = do
        sql <- execWriterT dsl
        return $ sql ++ ";"

validateTable :: MonadFail m => Table -> m ()
validateTable table =
    if validate table
    then return ()
    else fail "invalid table"
    where
        validate "" = False
        validate _ = True

validateDatabase :: MonadFail m => Table -> m ()
validateDatabase db =
    if validate db
    then return ()
    else fail "invalid database"
    where
        validate "" = False
        validate _ = True

nop :: MonadFail m => SQLDSL m
nop = return ()

select :: MonadFail m => [Column] -> SQLDSL m
select cols = do
    part <- toStrs cols
    tell $ "SELECT " ++ part ++ " "
    where
        toStrs [c] = return c
        toStrs (c:cs) = do
            part <- toStrs cs
            return $ c ++ "," ++ part
        toStrs _ = fail "columns cannot be empty"

from :: MonadFail m => Table -> SQLDSL m
from table = do
    validateTable table
    tell $ "FROM " ++ table ++ " "

where_ :: MonadFail m => Condition -> SQLDSL m
where_ (csql, _) = do
    tell $ "WHERE " ++ csql ++ " "

insert :: MonadFail m => Table -> [Column] -> SQLDSL m
insert table recs = do
    validateTable table
    part <- toStrs recs
    tell $ "INSERT INTO " ++ table ++ " (" ++
            part ++
            ") "
    where
        toStrs [c] = return c
        toStrs (c:cs) = do
            part <- toStrs cs
            return $ c ++ "," ++ part
        toStrs _ = fail "columns cannot be empty"

values :: MonadFail m => [SqlValue] -> SQLDSL m
values vals = do
    part <- toPhs vals
    tell $ "VALUES (" ++ part ++ ") "
    where
        toPhs [_] = return "?"
        toPhs (_:vs) = do
            part <- toPhs vs
            return $ "?," ++ part
        toPhs _ = fail "values cannot be empty"

update :: MonadFail m => Table -> SQLDSL m
update table = do
    validateTable table
    tell $ "UPDATE " ++ table ++ " "

set :: MonadFail m => [Record] -> SQLDSL m
set recs = do
    part <- toSubs recs
    tell $ "SET " ++ part ++ " "
    where
        toSub (c,_) = return $ c ++ " = ?"
        toSubs [r] = toSub r
        toSubs (r:rs) = do
            part1 <- toSub r
            part2 <- toSubs rs
            return $ part1 ++ "," ++ part2
        toSubs _ = fail "records cannot be empty"

delete :: MonadFail m => Table -> SQLDSL m
delete table = do
    validateTable table
    tell $ "DELETE FROM " ++ table ++ " "

createDatabase :: MonadFail m => Database -> SQLDSL m
createDatabase db = do
    validateDatabase db
    tell $ "CREATE DATABASE " ++ db ++ " "

createTable :: MonadFail m => Table -> [Definition] -> SQLDSL m
createTable table defs = do
    validateTable table
    part <- toDefs defs
    tell $ "CREATE TABLE " ++ table ++ " (" ++
            part ++
            ") "
    where
        toDef (c,t) = return $ c ++ " " ++ t
        toDefs [d] = toDef d
        toDefs (d:ds) = do
            part1 <- toDef d
            part2 <- toDefs ds
            return $ part1 ++ "," ++ part2
        toDefs _ = fail "definitions cannot be empty"

runSelect :: IConnection conn => Table -> [Column] -> Condition-> Runtime conn [[Record]]
runSelect table cols cond@(_,cvals) conn = do
    sql <- buildSQL $ do
                select cols
                from table
                where_ cond
    runQuery sql cvals cols conn

runInsert :: IConnection conn => Table -> [Record] -> Runtime conn Integer
runInsert table recs conn = do
    sql <- buildSQL $ do
                insert table cols
                values vals
    runSQL sql vals conn
    where
          (cols, vals) = unzip recs

runUpdate :: IConnection conn => Table -> [Record] -> Condition -> Runtime conn Integer
runUpdate table recs cond@(_,cvals) conn = do
    sql <- buildSQL $ do
                update table
                set recs
                where_ cond
    runSQL sql (vals <> cvals) conn
    where
          (_, vals) = unzip recs

runDelete :: IConnection conn => Table -> Condition -> Runtime conn Integer
runDelete table cond@(_,cvals) conn = do
    sql <- buildSQL $ do
                delete table
                where_ cond
    runSQL sql cvals conn
