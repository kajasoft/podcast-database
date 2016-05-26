{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, QuasiQuotes, FlexibleInstances #-} 
module Podcast.Database where
import Podcast.Types
import Database.PostgreSQL.Simple
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
import Data.Int (Int64)


fetchFeeds :: Connection -> [Int64] -> IO [EntityFeed]
fetchFeeds c ids = do
    let q = [sql|
              SELECT feed_url, feed_title, feed_link, feed_itunes_url, 
                feed_description, feed_last_build_date,
                feed_explicit, feed_keywords, feed_categories, feed_summary
              FROM feeds WHERE feed_id IN ?
              |]
    xs <- query c q $ Only (In ids)
    -- Order by original ids
    let xs' = M.fromList $ map (\x@EntityFeed{..} -> (feedId, x)) xs
    let xs'' = catMaybes $ map (\i -> M.lookup i xs') ids
    return xs''

insertFeed :: Connection -> Feed -> IO Int64
insertFeed c feed = do
    execute c [sql| INSERT INTO feeds 
          (feed_url, feed_title, feed_link, feed_itunes_url, 
          feed_description, feed_last_build_date,
          feed_explicit, feed_keywords, feed_categories, feed_summary)
          VALUES 
          (?, ?, ?, ?, ?, 
           ?, ?, ?, ?, ?) |]
          feed

doesFeedExist :: Connection -> Text -> IO (Maybe Int64)
doesFeedExist c feedURL' = do
    r :: [(Only Int64)] <- query c "select feed_id from feeds where feed_url = ?" (Only feedURL')
    case r of
      [(Only x)] -> return $ Just x
      _          -> return Nothing

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


