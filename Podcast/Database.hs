{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, QuasiQuotes #-} 
module Podcast.Database where
import Podcast.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
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


