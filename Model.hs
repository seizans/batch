{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Model where

import Data.Monoid
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

intToAgeArea :: Int -> AgeArea
intToAgeArea n
    | n < 20 = Over0
    | n < 40 = Over20
    | n < 60 = Over40
    | otherwise = Over60

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
  deriving (Show, Eq, Ord)

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

fromAllNumPerAge :: AllNumPerAge -> [NumPerAge]
fromAllNumPerAge all
    = NumPerAge Over0  Male   (over0Male    all)
    : NumPerAge Over0  Female (over0Female  all)
    : NumPerAge Over20 Male   (over20Male   all)
    : NumPerAge Over20 Female (over20Female all)
    : NumPerAge Over40 Male   (over40Male   all)
    : NumPerAge Over40 Female (over40Female all)
    : NumPerAge Over60 Male   (over60Male   all)
    : NumPerAge Over60 Female (over60Female all)
    : []

derivePersistField "Sex"
derivePersistField "AgeArea"
