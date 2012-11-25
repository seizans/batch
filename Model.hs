{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Model where

import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
    age Int
    sex Sex
  deriving Show

NumPerAge
    ageArea AgeArea
    sex Sex
    number Int
  deriving Show
|]

data Sex = Male | Female
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data AgeArea = Over0 | Over20 | Over40 | Over60
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "Sex"
derivePersistField "AgeArea"
