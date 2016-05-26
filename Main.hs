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

data Command = InsertFeed 

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
    command "insert-feed" (info (helper <*> insertFeedCmd) (progDesc "Insert Feed from JSON"))
  )

opts :: ParserInfo Options
opts = info (helper <*> options)
            (header "podcast-database" <> fullDesc)

insertFeedCmd :: Parser Command
insertFeedCmd = pure InsertFeed

main = do
    Options{..} <- execParser opts
    case optCmd of
      InsertFeed -> do
          -- make sure wrapper script limits input to top of file where feed json is
          s <- BL.getContents
          let f :: Feed
              f = maybe (error $ "Could not parse Feed: " ++ show s)
                        id $ decode s
          c <- getConnection optDatabaseName
          feedId' <- do 
                  feedId'' <- doesFeedExist c (chURL f)
                  maybe (insertFeed c f) return $ feedId''
          print feedId'


getConnection :: String -> IO Connection
getConnection dbname = 
    connectPostgreSQL $ "dbname=" <> (B.pack dbname)
