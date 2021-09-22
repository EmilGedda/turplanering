{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Either
import           Data.Foldable
import           Data.Geospatial
import           Data.LineString
import           Data.List
import           Data.Text.Read
import           GHC.Generics
import           Optics
import           Text.Printf
import           Turplanering.Collections
import qualified Data.ByteString          as B
import qualified Data.IntMap.Strict       as M
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Builder   as TB
import qualified Data.Text.Lazy.IO        as TIO
import qualified Data.Text.Lazy.Read      as TR

data Category = Category
    { cat_color :: Maybe T.Text
    , cat_name :: Maybe T.Text
    , cat_label :: Maybe T.Text
    }
    deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Category)

data FeatureDefinition = FeatureDefinition
    { children :: [T.Text]
    , typeKey :: Maybe T.Text
    , name :: T.Text
    , _type :: T.Text
    , description :: Maybe T.Text
    , mainCategory :: Category
    }
    deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')} ''FeatureDefinition)

newtype FeatureShape = FeatureShape {geotype :: T.Text}
    deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''FeatureShape)

data GeoDefinition = GeoDefinition
    { parent :: T.Text
    , color :: Maybe T.Text
    , inactive :: Bool
    , shape :: FeatureShape
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data FeatureType
    = Parent FeatureDefinition
    | Child GeoDefinition
    deriving (Show, Generic)

$(deriveJSON defaultOptions{sumEncoding = UntaggedValue} ''FeatureType)

isParent :: FeatureType -> Bool
isParent (Parent _) = True
isParent _ = False

fromParent :: FeatureType -> FeatureDefinition
fromParent (Parent def) = def
fromParent _ = error "fromParent: child is not a parent"

fromChild :: FeatureType -> GeoDefinition
fromChild (Child def) = def
fromChild _ = error "fromChild: parent is not a child"

idToInt :: FeatureID -> Int
idToInt (FeatureIDText t) = let Right (n, _) = decimal t in n
idToInt (FeatureIDNumber n) = n

newline :: TB.Builder
newline = TB.singleton '\n'

mergeLine :: GeospatialGeometry -> GeoMultiLine -> GeoMultiLine
mergeLine geom = over (lensVL unGeoMultiLine) $
    case geom of
        (MultiLine (GeoMultiLine multiline)) -> (<> multiline)
        (Line (GeoLine line)) -> (|> line)
        _ -> id

transaction :: TB.Builder -> TB.Builder
transaction sql =
    TB.fromString "BEGIN;"
        <> newline
        <> newline
        <> sql
        <> newline
        <> newline
        <> TB.fromString "COMMIT;"

lineString :: LineString GeoPositionWithoutCRS -> TB.Builder
lineString line =
    TB.fromText "LINESTRING ("
        <> mconcat (intersperse (TB.fromText ", ") . map point $ toList line)
        <> TB.singleton ')'
    where
        point (GeoPointXY (PointXY x y)) = TB.fromString $ printf "%.6f %.6f" x y
        point _ = error "point: LineString contained something other than GeoPointXY"

joinWith :: Monoid c => c -> [c] -> c
joinWith f = mconcat . intersperse f

preparedStatement :: TB.Builder
preparedStatement =
    TB.fromText
        "PREPARE insert_trail (text, text, text, geography[]) AS\n\
        \    WITH trail AS (\n\
        \        INSERT INTO\n\
        \            trails (name, description, color)\n\
        \        VALUES\n\
        \            ($1, $2, $3)\n\
        \        RETURNING id\n\
        \    )\n\
        \    INSERT INTO\n\
        \        trail_sections (trail_id, geog)\n\
        \    SELECT trail.id, UNNEST($4)\n\
        \    FROM trail;\n"

trailSql :: FeatureDefinition -> GeoMultiLine -> TB.Builder
trailSql trail (GeoMultiLine lines) =
    text
        "EXECUTE insert_trail( "
        <> q (Just . name)
        <> arg
        <> q description
        <> arg
        <> q (cat_color . mainCategory)
        <> arg
        <> text "ARRAY[ "
        <> sections
        <> text "]::geography[]);\n"
    where
        q f = maybe "null" quote (f trail)
        text = TB.fromLazyText
        arg = text "\n                    , "
        linestr = text "\n                           , "
        quote q =
            text "E'"
                <> (text . T.replace "'" "\\'" . T.replace "\n" "\\n" . T.filter (/= '\r') $ q)
                <> text "'"
        sections =
            joinWith linestr
                . map (\l -> text "'" <> lineString l <> text "'")
                $ toList lines

insertTrail :: FeatureDefinition -> [GeospatialGeometry] -> TB.Builder
insertTrail trail sections = trailSql trail multiline
    where
        multiline = foldr mergeLine (GeoMultiLine mempty) sections

trailsAndSections :: [GeoFeature FeatureType] -> TB.Builder
trailsAndSections features = joinWith newline $ map prepareTrail trails
    where
        (parents, children) = partition (isParent . _properties) features
        mapping =
            fst . fromRight (-1, mempty) . TR.decimal . parent . fromChild . _properties
                &&& return . _geometry

        sections :: M.IntMap [GeospatialGeometry]
        sections =
            M.fromListWith (<>) . toList
                . fmap mapping
                $ nubSortOn (fmap idToInt . _featureId) children

        trails :: [GeoFeature FeatureType]
        trails =
            nubSortOn (fmap idToInt . _featureId)
                . filter ((==) (T.pack "trail") . _type . fromParent . _properties)
                $ toList parents

        prepareTrail :: GeoFeature FeatureType -> TB.Builder
        prepareTrail feature =
            case _featureId feature of
                Nothing -> mempty
                Just featureId ->
                    insertTrail (fromParent $ _properties feature) $
                        M.findWithDefault [] (idToInt featureId) sections

main :: IO ()
main = do
    json <- B.getContents
    case eitherDecodeStrict' json of
        Left err -> putStrLn err
        Right coll ->
            let features = toList $ _geofeatures coll
             in TIO.putStrLn
                    . TB.toLazyText
                    . transaction
                    $ mconcat
                        [ preparedStatement
                        , newline
                        , trailsAndSections features
                        ]
