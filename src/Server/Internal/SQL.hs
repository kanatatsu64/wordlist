{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Internal.SQL (
    contTransaction,
    existTable,
    runExistTable,
    failFromSql,
    Runtime,
    runRuntime,
    getConn,
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
    columns,
    lookup,
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
    runExist,
    runSelect,
    runSelectAll,
    runInsert,
    runUpdate,
    runDelete,
    runDeleteAll,
    runCreateDatabase,
    runCreateTable,
    module Database.HDBC
) where

import Prelude hiding ( lookup )
import qualified Prelude

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Writer
import Database.HDBC

import Convertible ( Convertible (..), ConvertError (..) )
import Utils ( maybeToFail )

contTransaction conn = cont $ withTransaction conn

existTable :: IConnection conn => conn -> String -> IO Bool
existTable conn name = elem name <$> getTables conn

failFromSql :: (MonadFail m, Convertible SqlValue a) => SqlValue -> m a
failFromSql sval = case safeFromSql sval of
                      Right val -> return val
                      Left error -> fail $ convErrorMessage error

type Runtime c a = ReaderT c IO a

toRuntime :: (c -> IO a) -> Runtime c a
toRuntime = ReaderT

runRuntime :: Runtime c a -> c -> IO a
runRuntime = runReaderT

getConn :: IConnection conn => Runtime conn conn
getConn = ask

runSQL :: IConnection conn => SQL -> [SqlValue] -> Runtime conn Integer
runSQL sql vals = toRuntime $ go sql vals
    where
        go sql vals conn = run conn sql vals

runQuery :: IConnection conn => SQL -> [SqlValue] -> [Column] -> Runtime conn [[Record]]
runQuery sql vals cols = toRuntime $ go sql vals cols
    where
        go sql vals cols conn = do
            rows <- quickQuery' conn sql vals
            return $ zip cols <$> rows

runExistTable :: IConnection conn => Table -> Runtime conn Bool
runExistTable table = do
    conn <- getConn
    liftIO $ existTable conn table

type SQL = String
type Database = String
type Table = String
type Column = String
type Condition = (SQL, [SqlValue])
type DataType = String
type Definition = (Column, DataType)

type Record = (Column, SqlValue)

class ISchema a where
    table :: a -> Table
    definitions :: a -> [Definition]
    toRecords :: a -> [Record]
    fromRecords :: [Record] -> Maybe a

columns :: ISchema a => a -> [Column]
columns schema = map fst (definitions schema)

lookup :: MonadFail m => Convertible SqlValue a => Column -> [Record] -> m a
lookup col recs = do
    sval <- maybeToFail message $ Prelude.lookup col recs
    failFromSql sval
    where message = "record is not found: " ++ col

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

runExist :: IConnection conn => Table -> Condition -> Runtime conn Bool
runExist table cond@(_, cvals) = do
    sql <- buildSQL $ do
                select ["COUNT(*)"]
                from table
                where_ cond
    recs <- runQuery sql cvals ["count"]
    let ("count", sval) = head . head $ recs
        val = fromSql @Int sval
    return $ val > 0

runSelect :: IConnection conn => Table -> [Column] -> Condition-> Runtime conn [[Record]]
runSelect table cols cond@(_,cvals) = do
    sql <- buildSQL $ do
                select cols
                from table
                where_ cond
    runQuery sql cvals cols

runSelectAll :: IConnection conn => Table -> [Column] -> Runtime conn [[Record]]
runSelectAll table cols = do
    sql <- buildSQL $ do
                select cols
                from table
    runQuery sql [] cols

runInsert :: IConnection conn => Table -> [Record] -> Runtime conn Integer
runInsert table recs = do
    sql <- buildSQL $ do
                insert table cols
                values vals
    runSQL sql vals
    where
          (cols, vals) = unzip recs

runUpdate :: IConnection conn => Table -> [Record] -> Condition -> Runtime conn Integer
runUpdate table recs cond@(_,cvals) = do
    sql <- buildSQL $ do
                update table
                set recs
                where_ cond
    runSQL sql (vals <> cvals)
    where
          (_, vals) = unzip recs

runDelete :: IConnection conn => Table -> Condition -> Runtime conn Integer
runDelete table cond@(_,cvals) = do
    sql <- buildSQL $ do
                delete table
                where_ cond
    runSQL sql cvals

runDeleteAll :: IConnection conn => Table -> Runtime conn Integer
runDeleteAll table = do
    sql <- buildSQL $ delete table
    runSQL sql []

runCreateDatabase :: IConnection conn => Database -> Runtime conn Integer
runCreateDatabase database = do
    sql <- buildSQL $ createDatabase database
    runSQL sql []

runCreateTable :: IConnection conn => Table -> [Definition] -> Runtime conn Integer
runCreateTable table defs = do
    sql <- buildSQL $ createTable table defs
    runSQL sql []
