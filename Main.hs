{-# LANGUAGE OverloadedStrings #-}

import Database.Persist.Sqlite
import Model

main :: IO ()
main = do
    initDB
  where
    dbn = "batch.sqlite3"
    initDB = withSqliteConn dbn $ runSqlConn $ runMigration migrateAll
