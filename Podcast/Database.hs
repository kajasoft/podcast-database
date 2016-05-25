{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, QuasiQuotes, FlexibleInstances #-} 
module Podcast.Database where
import Podcast.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text.Read (decimal, double)
import Data.Monoid
import Podcast.Types

data EntityFeed = EntityFeed {
      feedId :: Int
    , eFeed :: Feed
    }

-- TODO CHANGEME
baseQuery = [sql|
        select title_id, title_uri, title, year, content_type, 
        maturity_rating_value, synopsis,
        actors_json, directors_json, creators_json, genres_json,
        ((average_rating * 10)::integer) as average_rating,
        awards_json, languages,
        webpage, tinyurl, runtime,
        series_id, series_title, season_id, season_title, season_number, 
        program_sequence, program_count, children_ids, similars_ids, 
        available_from,
        expires_on,
        is_hd, is_super_hd, is_ultra_hd, box_art_json,
        rt_id,
        rt_critics_rating, rt_critics_score,
        rt_audience_rating, rt_audience_score, 
        nyt_review_id, nyt_critics_pick,
        queue_count, queue_count_today
        from titles |]

fetchFeeds :: Connection -> [Int] -> IO [EntityFeed]
fetchFeeds c ids = do
    let q = (baseQuery <> " where feed_id in ?")
    xs <- query c q $ Only (In ids)
    -- order by original ids
    let xs' = M.fromList $ map (\x@EntityFeed{..} -> (feedId, x)) xs
    let xs'' = catMaybes $ map (\i -> M.lookup i xs') ids
    return xs''

instance FromRow EntityFeed where
  fromRow = EntityFeed
    <$> field
    <*> (Feed 
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> (T.splitOn "," <$> field) -- keywords
        <*> (T.splitOn "," <$> field) -- categories
        <*> field
        )

instance ToRow Feed where
  toRow Feed{..} = [
      toField chTitle
    , toField chLink
    , toField chDescription
    , toField chLastBuildDate
    , toField chExplicit
    , toField $ T.intercalate "," chKeywords
    , toField $ T.intercalate "," chCategories
    , toField chSummary
    ]


------------------------------------------------------------------------

decodeIds :: Maybe Text -> [Int]
decodeIds s =
    case s of 
      Nothing -> []
      Just s' -> let xs = T.splitOn "," s'
                 in map (flip decimalFromText 0) xs


decimalFromText :: Text 
                -> Int    -- ^ default 
                -> Int
decimalFromText x def = either (const def) fst $ decimal x


