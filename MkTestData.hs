{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MkTestData where

import Control.Applicative
import Data.Monoid
import Database.Persist (Filter, PersistEntity, PersistQuery)
import qualified Database.Persist as DB
import Database.Persist.GenericSql (runMigration)

import DbUtil (runSqlite)
import Model

main :: IO ()
main = runSqlite $ do
    runMigration migrateAll
    deleteAndInsert testDataPerson
    deleteAndInsert initialDataNumPerAge

deleteAndInsert
    :: forall a m backend.(PersistQuery backend m, PersistEntity a)
    => [a]
    -> backend m ()
deleteAndInsert xs = do
    DB.deleteWhere ([] :: [Filter a])
    mapM_ DB.insert xs

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

initialDataNumPerAge :: [NumPerAge]
initialDataNumPerAge
    = NumPerAge
    <$> [Over0, Over20, Over40, Over60]
    <*> [Male, Female]
    <*> [0]
