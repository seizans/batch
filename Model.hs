{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleContexts, GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Model where

import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
    age Int Maybe
  deriving Show
|]
