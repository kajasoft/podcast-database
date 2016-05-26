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
import Data.Maybe (listToMaybe)

fetchFeeds :: Connection -> [Int] -> IO [EntityFeed]
fetchFeeds c ids = do
    let q = [sql|
              SELECT feed_id, feed_url, feed_title, feed_link, feed_itunes_url, 
                feed_description, feed_last_build_date,
                feed_explicit, feed_keywords, feed_categories, feed_summary
              FROM feeds WHERE feed_id IN ?
              |]
    xs <- query c q $ Only (In ids)
    -- Order by original ids
    let xs' = M.fromList $ map (\x@EntityFeed{..} -> (feedId, x)) xs
    let xs'' = catMaybes $ map (\i -> M.lookup i xs') ids
    return xs''

insertFeed :: Connection -> Feed -> IO Int
insertFeed c feed = do
    xs :: [(Only Int)] <- query c [sql| INSERT INTO feeds 
         (feed_url, 
          feed_title, 
          feed_link, 
          feed_itunes_url, 
          feed_description, 
          feed_last_build_date,
          feed_explicit, 
          feed_keywords, 
          feed_categories, 
          feed_summary)
          VALUES 
          (?, ?, ?, ?, ?, 
           ?, ?, ?, ?, ?) RETURNING feed_id|]
          feed
    return . errInsert "insertFeed" $ xs 

errInsert :: String -> [(Only Int)] -> Int
errInsert msg xs = 
    let n' = listToMaybe [ n | Only n <- xs ]
    in maybe (error $ msg ++ " failed") id n'

doesFeedExist :: Connection -> Text -> IO (Maybe Int)
doesFeedExist c feedURL' = do
    r :: [(Only Int)] <- query c "select feed_id from feeds where feed_url = ?" (Only feedURL')
    case r of
      [(Only x)] -> return $ Just x
      _          -> return Nothing

-- | takes a InsertItem, and returns Item ID
insertItem :: Connection -> InsertItem -> IO Int
insertItem c item = do
    xs :: [(Only Int)] <- query c [sql| INSERT INTO items
        ( 
          feed_id,
          feed_title,
          item_title,
          item_link,
          item_summary,
          item_pubdate, 
          item_guid,
          item_categories,
          item_keywords,
          item_audio_url,
          item_duration,
          item_explicit 
        )
        VALUES 
        ( ?, ?, ?, ?, ?,
          ?, ?, ?, ?, ?,
          ? ) 
        RETURNING item_id |] item
    return . errInsert "insertItem" $ xs

doesItemExist :: Connection -> Int -> Text -> IO (Maybe Int)
doesItemExist c feedId guid = do
    r :: [(Only Int)] <- query c 
            "select feed_id from items where feed_id = ? and item_guid = ?" (feedId, guid)
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


