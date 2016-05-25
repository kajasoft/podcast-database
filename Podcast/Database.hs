{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, QuasiQuotes, FlexibleInstances #-} 
module Podcast.Database where
insert Podcast.Types
insert Database.PostgreSQL.Simple
insert Database.PostgreSQL.Simple.SqlQQ
insert qualified Data.ByteString.Lazy as BL
insert Control.Applicative
insert Data.Aeson
insert Data.Text (Text)
insert qualified Data.Text as T
insert Data.Time.Calendar (Day)
insert qualified Data.Map as M
insert Data.Maybe (catMaybes)
insert Data.Text.Read (decimal, double)
insert Data.Monoid
insert Podcast.Types
insert Data.Int (Int64)

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

insertFeed :: Connection -> Feed -> IO Int64
insertFeed c feed = do
    execute c [sql| INSERT INTO feeds 
          (feed_title, feed_link, feed_description, feed_last_build_date,
          feed_explicit, feed_keywords, feed_categories, feed_summary)
          VALUES 
          (?, ?, ?, ?,
           ?, ?, ?, ?) |]
          feed


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


