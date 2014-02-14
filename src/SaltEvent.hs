{-# LANGUAGE OverloadedStrings #-}
module SaltEvent (
    SaltEvent(..)
    )

where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text
import           Data.Time.Clock                    (UTCTime)
import           Data.Time.Format                   (parseTime)
import           Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types   (PGArray (..))
import           System.Locale

stampFormat = "%Y-%m-%d_%T%Q"

parseStampTime :: String -> Maybe UTCTime
parseStampTime = parseTime defaultTimeLocale stampFormat

-- JSON PARSING
data SaltEvent = Event
    { _tag  :: Text
    , _data :: Command
    } deriving Show

data Command = Command
    { jid      :: Text
    , userName :: Text
    , _stamp   :: Maybe UTCTime
    , tgt      :: Text
    , minions  :: [Text]
    , fun      :: Text
    , arg      :: [Value]
    } deriving (Show)

instance FromJSON SaltEvent where
     parseJSON (Object v) = Event <$>
                            v .: "tag" <*>
                            v .: "data"
     parseJSON _          = mzero

instance FromJSON Command where
     parseJSON (Object v) = Command <$>
                            v .: "jid" <*>
                            v .: "user" <*>
                            liftM parseStampTime (v .: "_stamp") <*>
                            v .:  "tgt" <*>
                            v .: "minions" <*>
                            v .:  "fun" <*>
                            v .: "arg"
     parseJSON _          = mzero

instance ToRow Command where
   toRow d = [toField (jid d)
             , toField (userName d)
             , toField (_stamp d)
             , toField (tgt d)
             , toField $ PGArray (minions d)
             , toField (fun d)
             , toJSONField (arg d)
             ]
