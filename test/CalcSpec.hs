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

instance Arbitrary AllNumPerAge where
    arbitrary = return AllNumPerAge
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary

size :: AllNumPerAge -> Int
size x = (over0Male x)
    + (over0Female x)
    + (over20Male x)
    + (over20Female x)
    + (over40Male x)
    + (over40Female x)
    + (over60Male x)
    + (over60Female x)

numOfPerson :: [NumPerAge] -> Int
numOfPerson = foldr sum 0
  where
    sum :: NumPerAge -> Int -> Int
    sum (NumPerAge _ _ x) y  = x + y

prop_calculateNumPerAge = \xs -> (over0Female $ calculateNumPerAge xs) == 0

filterMale :: [Person] -> [Person]
filterMale xs = [x | x <- xs, (personSex x) == Female]

filterFemale :: [Person] -> [Person]
filterFemale xs = [x | x <- xs, (personSex x) == Male]

spec :: Spec
spec = do
    describe "calculateNumPerAge" $ do
        prop "input with Males" $ \xs ->
            (over0Female $ calculateNumPerAge $ filterFemale xs) == 0
        prop "input with Females" $ \xs ->
            (over0Female $ calculateNumPerAge $ filterMale xs) == 0

    describe "fromAllNumPerAge" $ do
        prop "whole size should be the same" $ \x ->
            (size x) == (numOfPerson $ fromAllNumPerAge x)
