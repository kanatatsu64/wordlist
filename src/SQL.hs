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
    module Database.HDBC,

    selectBuilder,
    fromBuilder,
    whereBuilder,
    insertBuilder,
    valuesBuilder,
    updateBuilder,
    setBuilder,
    deleteBuilder,
    createTableBuilder,
    createDatabaseBuilder
) where

import Control.Monad.Cont
import Control.Monad.Writer
import Database.HDBC
import Data.Convertible ( Convertible )
import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )

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

execRuntime :: Runtime c a -> c -> IO a
execRuntime = id

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

type SQLDSL = Writer SQL ()

buildSQL :: SQLDSL -> SQL
buildSQL dsl = execWriter dsl ++ ";"

nop :: SQLDSL
nop = return ()

select :: [Column] -> SQLDSL
select cols = tell $ selectBuilder cols

selectBuilder :: [Column] -> SQL
selectBuilder cols = sql
    where sql = "SELECT " ++ toStrs cols ++ " "
          toStrs [c] = c
          toStrs (c:cs) = c ++ "," ++ toStrs cs

from :: Table -> SQLDSL
from table = tell $ fromBuilder table

fromBuilder :: Table -> SQL
fromBuilder table = sql
    where sql = "FROM " ++ table ++ " "

where_ :: Condition -> SQLDSL
where_ cond = tell $ whereBuilder cond

whereBuilder :: Condition -> SQL
whereBuilder (csql,_) = "WHERE " ++ csql ++ " "

insert :: Table -> [Column] -> SQLDSL
insert table recs = tell $ insertBuilder table recs

insertBuilder :: Table -> [Column] -> SQL
insertBuilder table cols = sql
    where sql = "INSERT INTO " ++ table ++ " (" ++
                toStrs cols ++
                ") "
          toStrs [c] = c
          toStrs (c:cs) = c ++ "," ++ toStrs cs

values :: [SqlValue] -> SQLDSL
values vals = tell $ valuesBuilder vals

valuesBuilder :: [SqlValue] -> SQL
valuesBuilder vals = sql
    where sql = "VALUES (" ++ toPhs vals ++ ") "
          toPhs [_] = "?"
          toPhs (_:vs) = "?," ++ toPhs vs

update :: Table -> SQLDSL
update table = tell $ updateBuilder table

updateBuilder :: Table -> SQL
updateBuilder table = "UPDATE " ++ table ++ " "

set :: [Record] -> SQLDSL
set recs = tell $ setBuilder recs

setBuilder :: [Record] -> SQL
setBuilder recs = sql
    where sql = "SET " ++ toSubs recs ++ " "
          toSub (c,_) = c ++ " = ?"
          toSubs [r] = toSub r
          toSubs (r:rs) = toSub r ++ "," ++ toSubs rs

delete :: Table -> SQLDSL
delete table = tell $ deleteBuilder table

deleteBuilder :: Table -> SQL
deleteBuilder table = "DELETE FROM " ++ table ++ " "

createDatabase :: Database -> SQLDSL
createDatabase db = tell $ createDatabaseBuilder db

createDatabaseBuilder :: Database -> SQL
createDatabaseBuilder db = "CREATE DATABASE " ++ db ++ " "

createTable :: Table -> [Definition] -> SQLDSL
createTable table defs = tell $ createTableBuilder table defs

createTableBuilder :: Table -> [Definition] -> SQL
createTableBuilder table defs = sql
    where sql = "CREATE TABLE " ++ table ++ " (" ++
                toDefs defs ++
                ") "
          toDef (c,t) = c ++ " " ++ t
          toDefs [d] = toDef d
          toDefs (d:ds) = toDef d ++ "," ++ toDefs ds

runSelect :: IConnection conn => Table -> [Column] -> Condition-> Runtime conn [[Record]]
runSelect table cols cond@(_,cvals) = runQuery sql cvals cols
    where sql = buildSQL $ do
                    select cols
                    from table
                    where_ cond

runInsert :: IConnection conn => Table -> [Record] -> Runtime conn Integer
runInsert table recs = runSQL sql vals
    where sql = buildSQL $ do
                    insert table cols
                    values vals
          (cols, vals) = unzip recs

runUpdate :: IConnection conn => Table -> [Record] -> Condition -> Runtime conn Integer
runUpdate table recs cond@(_,cvals) = runSQL sql (vals <> cvals)
    where sql = buildSQL $ do
                    update table
                    set recs
                    where_ cond
          (_, vals) = unzip recs

runDelete :: IConnection conn => Table -> Condition -> Runtime conn Integer
runDelete table cond@(_,cvals) = runSQL sql cvals
    where sql = buildSQL $ do
                    delete table
                    where_ cond
