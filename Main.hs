{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Monoid
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
    --mapM_ DB.insert initialData
  where
    initialData = NumPerAge <$> [Over0, Over20, Over40, Over60] <*> [Male, Female] <*> [0]

execSqlite
  :: SqlPersist IO a
  -> IO a
execSqlite = withSqliteConn dbn . runSqlConn
  where
    dbn = "batch.sqlite3"

calculateNumPerAge :: [Person] -> AllNumPerAge
calculateNumPerAge xs = helper xs mempty
  where
    helper :: [Person] -> AllNumPerAge -> AllNumPerAge
    helper [] acc = acc
    helper (p:ps) acc = helper ps (app p acc)
    app p acc = case personSex p of
        Male -> case (intToAgeArea (personAge p)) of
            Over0 -> acc { over0Male = 1 + over0Male acc }
            Over20 -> acc { over20Male = 1 + over20Male acc }
            Over40 -> acc { over40Male = 1 + over40Male acc }
            Over60 -> acc { over60Male = 1 + over60Male acc }
        Female -> case (intToAgeArea (personAge p)) of
            Over0 -> acc { over0Female = 1 + over0Female acc }
            Over20 -> acc { over20Female = 1 + over20Female acc }
            Over40 -> acc { over40Female = 1 + over40Female acc }
            Over60 -> acc { over60Female = 1 + over60Female acc }

main :: IO ()
main = execSqlite $ do
    runMigration migrateAll
    initNumPerAge
    mapM_ DB.insert testDataPerson
    personEntities <- selectList [] []
    let persons = map entityVal personEntities
    mapM_ DB.insert $ fromAllNumPerAge $ calculateNumPerAge persons
