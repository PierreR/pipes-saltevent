{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main
where

import Data.Time.Format     (parseTime)
--import System.Locale        (defaultTimeLocale)
import Data.Time.Clock      (UTCTime)
import           Control.Applicative
import           Control.Lens        (view)
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))
import           GHC.Generics
import           Pipes
import qualified Pipes.Aeson         as PAe
import qualified Pipes.ByteString    as PB
import           Pipes.Group
import           Pipes.HTTP
import           Pipes.Parse
import System.Locale
-- Removing "data: " by brute force for now
jsonLowerBound :: Int
jsonLowerBound = 6

token = "edf5e44d8c12852af87d62423c21bf7f"
serverUrl = "http://localhost:8080/event/" ++ token ++ "?tag=salt%2Fjob%2F"
stampFormat = "%Y-%m-%d_%T%Q"

parseStampTime :: String -> Maybe UTCTime
parseStampTime = parseTime defaultTimeLocale stampFormat

insertSQL = "insert into event (jid, username, stamp, tgt, minions, fun, arg) values (?, ?, ?, ?, ?, ?, ?)"

-- JSON PARSING
data Event = Event
    { _tag     :: String
    , _data :: Command
    } deriving Show

data Command = Command

    { jid       :: String
    , userName  :: String
    , _stamp  :: Maybe UTCTime
    , tgt     :: String
    , minions :: [String]
    , fun     :: String
    , arg     :: [Value]
    } deriving (Show, Generic)

--instance FromJSON Command

instance FromJSON Event where
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

instance Pg.ToRow Command where
   toRow d = [toField (jid d)
             , toField (userName d)
             , toField (_stamp d)
             , toField (tgt d)
             , toField $ PGArray (minions d)
             , toField (fun d)
             , toField $ PGArray (fmap toJSON (arg d))
             ]

-- lens getters are functions of Producers
getLines::
    Monad m
    => Producer ByteString m ()
    -> FreeT (Producer ByteString m) m ()
getLines = view PB.lines

processEvtStream :: MonadIO m => Pg.Connection -> Producer ByteString m () -> Producer Event m ()
processEvtStream conn = go . getLines
  where
    go :: MonadIO m => FreeT (Producer ByteString m) m r -> Producer Event m r
    go freeT = do
        x <- lift $ runFreeT freeT -- runFreeT to get the next line (the next Producer inside the FreeT)
        case x of
            Pure r -> -- There aren't no group left for instance when the server stops streaming events
                liftIO $ return r
            Free p -> do
                (jr, p') <- lift $ runStateT PAe.decode (p >-> PB.drop jsonLowerBound)
                case jr of
                    Left e    -> do -- json parser returns an error
                        liftIO $ print e
                        return ()
                    Right jv -> do
                        let cmd = _data jv
                        liftIO $ print cmd
                        liftIO $ Pg.execute conn insertSQL $ cmd
                        yield jv
                -- p' :: Producer ByteString m (FreeT (Producer ByteString m) m r)
                freeT' <- lift $ drain p'
                go freeT'

    drain :: Monad m => Producer a m r -> m r
    drain p = runEffect $ for p discard

main = do
    conn <- Pg.connect Pg.defaultConnectInfo { Pg.connectUser = "jules", Pg.connectDatabase = "jules", Pg.connectPassword = "jules"}
    req <- parseUrl "http://localhost:8080/event/edf5e44d8c12852af87d62423c21bf7f?tag=salt%2Fjob%2F"
    withManager defaultManagerSettings $ \m ->
       withHTTP req m $ \resp ->
            runEffect $ for (processEvtStream conn (responseBody resp)) (liftIO.print)
