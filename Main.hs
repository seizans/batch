{-# LANGUAGE OverloadedStrings #-}

import qualified Database.Persist as DB
import Database.Persist

import DbUtil (runSqlite)
import Model
import qualified Service as Service

main :: IO ()
main = runSqlite $ do
    personEntities <- selectList [] []
    let persons = map entityVal personEntities
    mapM_ DB.insert $ fromAllNumPerAge $ Service.calculateNumPerAge persons
