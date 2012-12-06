{-# LANGUAGE FlexibleInstances #-}

module CalcSpec where

import Test.Hspec
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import Model
import Logic.Categorize
import qualified Data.Text as DT

instance Arbitrary Person where
    arbitrary = return Person `ap` (DT.pack <$> arbitrary) `ap` arbitrary `ap` arbitrary

instance Arbitrary Sex where
    arbitrary = elements [Male]

prop_calculateNumPerAge = \xs -> (over0Female $ calculateNumPerAge xs) == 0

spec :: Spec
spec = do
    describe "test" $ do
        prop "input with Males" $ \xs ->
            (over0Female $ calculateNumPerAge xs) == 0
