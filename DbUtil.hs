{-# LANGUAGE OverloadedStrings #-}
module DbUtil where

import Database.Persist.GenericSql (SqlPersist, runSqlConn)
import Database.Persist.Sqlite (withSqliteConn)

runSqlite
  :: SqlPersist IO a
  -> IO a
runSqlite = withSqliteConn dbn . runSqlConn
  where
    dbn = "db.sqlite3"
