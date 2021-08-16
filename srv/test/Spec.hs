{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoDefaultSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Arrow
import Data.Coerce
import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Data.Map as M
import Data.Validity.Text ()
import Optics
import Test.Hspec
import Test.QuickCheck
import Turplanering.Collections
import Turplanering.DB
import Turplanering.DB.Trail ()
import Turplanering.PostGIS ()

newtype WrappedInt = Wrap Int
    deriving (Ord, Eq)

-- TODO: Proper type for `DBTrailData [DBTrail] [DBSections]`
-- to manage invariants and assert DB sanity
coherentDBTrailData :: [DBTrail] -> [DBSections] -> ([DBTrail], [DBSections])
coherentDBTrailData xs ys = (trails', sections')
    where
        trails' = nubSortOn (view #id) xs
        trailIds = trails' ^.. folded % #id
        sections' = filter (flip elem trailIds . view #id) ys

main :: IO ()
main = hspec $ do
    describe "bucketOn" $ do
        it "keeps unique values on keys" . forAll genValid $
            \xs -> M.elems (bucketOn Wrap xs) `shouldMatchList` nubSort xs

        it "key creation should be consistent" . forAll genValid $
            uncurry shouldBe . first (map coerce) . unzip . M.toList . bucketOn Wrap

    describe "buildTrails" $ do
        it "trails should be preserved" . forAll genValid $
            \(ts :: [DBTrail]) ->
                let has = buildTrails wants []
                    wants = nubSortOn (view #id) ts
                 in (has ^.. folded % #name)
                        `shouldMatchList` (wants ^.. folded % #name)

        it "sections should be preserved" . forAll genValid $
            \(xs, ys) ->
                let (trails, sections') = coherentDBTrailData xs ys
                    dbNames =
                        sections'
                            ^.. folded
                                % #name
                    sectionNames =
                        buildTrails trails sections'
                            ^.. folded
                                % #sections
                                % folded
                                % #name
                 in sectionNames `shouldMatchList` dbNames
