{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Model where

import Data.Monoid (Monoid(..))
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

data AllNumPerAge = AllNumPerAge
    { over0Male :: Int
    , over0Female :: Int
    , over20Male :: Int
    , over20Female :: Int
    , over40Male :: Int
    , over40Female :: Int
    , over60Male :: Int
    , over60Female :: Int
    }
instance Monoid AllNumPerAge where
    mempty = AllNumPerAge 0 0 0 0 0 0 0 0
    mappend x y = AllNumPerAge
        (over0Male x    + over0Male y)
        (over0Female x  + over0Female y)
        (over20Male x   + over20Male y)
        (over20Female x + over20Female y)
        (over40Male x   + over40Male y)
        (over40Female x + over40Female y)
        (over60Male x   + over60Male y)
        (over60Female x + over60Female y)

derivePersistField "Sex"
derivePersistField "AgeArea"
