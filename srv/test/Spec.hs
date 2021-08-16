{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Arrow
import           Data.GenValidity
import           Data.GenValidity.Text      ()
import           Data.Validity.Text         ()
import           Test.Hspec
import           Test.QuickCheck
import           Turplanering.Map
import           Turplanering.Collections
import           Turplanering.DB
import           Turplanering.PostGIS       ()
import qualified Data.Map                   as M
import qualified Turplanering.DB.Section    as Section
import qualified Turplanering.DB.Trail      as Trail
import Data.Coerce

newtype WrappedInt = Wrap Int
    deriving (Ord, Eq)

-- TODO: Proper type for `DBTrailData [DBTrail] [DBSections]`
-- to manage invariants and assert DB sanity
coherentDBTrailData :: [DBTrail] -> [DBSections] -> ([DBTrail], [DBSections])
coherentDBTrailData xs ys = (trails, sections)
    where trails = nubSortOn Trail.id xs
          sections = filter (flip elem (map Trail.id xs) . Section.trailId) ys

main :: IO ()
main = hspec $ do
    describe "bucketOn" $ do
        it "keeps unique values on keys" . forAll genValid
            $ \xs -> M.elems (bucketOn Wrap xs) `shouldMatchList` nubSort xs

        it "key creation should be consistent" . forAll genValid
            $ uncurry shouldBe . first (map coerce) . unzip . M.toList . bucketOn Wrap

    describe "buildTrails" $ do
        it "trails should be preserved" . forAll genValid
            $ \ts -> let trails = nubSortOn Trail.id ts
                     in  map trailName (buildTrails trails [])
                         `shouldMatchList` map Trail.name trails

        it "sections should be preserved" . forAll genValid
            $ \(xs, ys) -> let (trails, sections) = coherentDBTrailData xs ys
                           in  concatMap (map sectionName . trailSections)
                                         (buildTrails trails sections)
                               `shouldMatchList` map Section.name sections
