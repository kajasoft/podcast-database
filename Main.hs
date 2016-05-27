{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Podcast.Types
import Podcast.Database
import Options.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Database.PostgreSQL.Simple

data Options = Options {
    optDatabaseName :: String
  , optCmd :: Command
  }

data Command = InsertFeedCommand
             | InsertItemCommand Int
             | FetchItemsCommand

options :: Parser Options
options = Options
  <$> strOption (short 'd'
               <> metavar "DBNAME"
               <> value "podcasts"
               <> help "Default: podcasts"
               )
  <*> cmd


cmd :: Parser Command
cmd = subparser (
      command "insert-feed" (info (helper <*> pure InsertFeedCommand)   
                            (progDesc "Insert Feed from JSON on STDIN"))
   <> command "insert-item" (info (helper <*> insertItemCommand) 
                            (progDesc "Insert Item from JSON on STDIN"))
   <> command "items" (info (helper <*> pure FetchItemsCommand) 
                            (progDesc "Fetch item ids from STDIN and output JSON"))
  )

insertItemCommand :: Parser Command
insertItemCommand = InsertItemCommand <$> argument auto (metavar "FEED-ID")

opts :: ParserInfo Options
opts = info (helper <*> options)
            (header "podcast-database" <> fullDesc)


main = do
    Options{..} <- execParser opts
    c <- getConnection optDatabaseName

    case optCmd of
      InsertFeedCommand -> do
          -- make sure wrapper script limits input to top of file where feed json is
          s <- BL.getContents
          let f :: Feed
              f = maybe (error $ "Could not parse Feed: " ++ show s)
                        id $ decode s
          feedId' <- do 
                  feedId'' <- doesFeedExist c (chURL f)
                  maybe (insertFeed c f) return $ feedId''
          print feedId'
      InsertItemCommand feedId -> do
          lines <- BL.lines <$> BL.getContents
          [eFeed] <- fetchFeeds c [feedId]
          mapM_ (\line -> do
              let x :: Item
                  x = maybe (error $ "Could not parse Item: " ++ show line) id $ decode line
              itemId <- do
                          itemId <- doesItemExist c feedId (iGUID x) 
                          maybe (insertItem c (InsertItem eFeed x)) return $ itemId
              print itemId
              ) lines
      FetchItemsCommand -> do
          ids :: [Int] <- (map read . lines) <$> getContents
          items <- fetchItems c ids
          mapM_ print items

getConnection :: String -> IO Connection
getConnection dbname = 
    connectPostgreSQL $ "dbname=" <> (B.pack dbname)
