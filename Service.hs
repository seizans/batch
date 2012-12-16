{-# LANGUAGE OverloadedStrings #-}

module Service
    ( calculateNumPerAge
    ) where

import Control.Applicative
import Data.Monoid
import Database.Persist (Filter)
import qualified Database.Persist as DB
import Database.Persist.Sqlite
import Model

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
