{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Database.Persist (Filter)
import qualified Database.Persist as DB
import Database.Persist.Sqlite
import Model

testDataPerson :: [Person]
testDataPerson = map (\(x,y,z) -> Person x y z)
    [ ("PersonName1", 10, Male)
    , ("PersonName2", 11, Female)
    , ("PersonName3", 32, Male)
    , ("PersonName4", 33, Female)
    , ("PersonName5", 44, Male)
    , ("PersonName6", 45, Female)
    , ("PersonName7", 56, Male)
    , ("PersonName8", 57, Female)
    , ("PersonName9", 78, Male)
    , ("PersonName10", 79, Female)
    ]

-- Delete all records and insert initial record.
initNumPerAge
  :: PersistQuery backend m
  => backend m ()
initNumPerAge = do
    DB.deleteWhere ([] :: [Filter NumPerAge])
    mapM_ DB.insert initialData
  where
    initialData = NumPerAge <$> [0, 20, 40, 60] <*> [Male, Female] <*> [0]

main :: IO ()
main = do
    initDB
  where
    dbn = "batch.sqlite3"
    initDB = withSqliteConn dbn $ runSqlConn $ runMigration migrateAll
