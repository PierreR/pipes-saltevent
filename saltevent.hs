{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main
where

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
import GHC.Int
-- Removing "data: " by brute force for now
jsonLowerBound :: Int
jsonLowerBound = 6

token = "2b230e965c65eaf992962be7f0b0ab9b"
serverUrl = "http://localhost:8080/event/" ++ token ++ "?tag=salt%2Fjob%2F"

insertSQL = "insert into events (jid, user, stamp, tgt, minions, fun, arg) values (?, ?, ?, ?, ?, ?, ?)"

-- JSON PARSING
data Event = Event
    { _tag     :: String
    , _data :: Command
    } deriving Show

data Command = Command
    { jid       :: String
    , user    :: String
    , _stamp  :: String
    , tgt     :: String
    , minions :: [String]
    , fun     :: String
    , arg     :: [Value]
    } deriving (Show, Generic)

instance FromJSON Command

instance FromJSON Event where
     parseJSON (Object v) = Event <$>
                            v .: "tag" <*>
                            v .: "data"
     parseJSON _          = mzero

instance Pg.ToRow Command where
   toRow d = [toField (jid d), toField (user d), toField (_stamp d), toField (tgt d), toField $ PGArray (minions d), toField (fun d), toField $ PGArray (arg d)]

-- lens getters are functions of Producers
getLines::
    Monad m
    => Producer ByteString m ()
    -> FreeT (Producer ByteString m) m ()
getLines = view PB.lines

hello :: Pg.ToRow q => q -> IO (Int64)
hello q = do
   conn <- Pg.connect Pg.defaultConnectInfo
   Pg.execute conn "insert into users (first_name) values (?)" q

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
                    Left  _    -> -- json parser returns an error
                        return ()
                    Right jv -> do
                        let cmd = _data jv
                        liftIO $ Pg.execute conn insertSQL $ cmd
                        yield jv
                -- p' :: Producer ByteString m (FreeT (Producer ByteString m) m r)
                freeT' <- lift $ drain p'
                go freeT'

    drain :: Monad m => Producer a m r -> m r
    drain p = runEffect $ for p discard

main = do
    conn <- Pg.connect Pg.defaultConnectInfo { Pg.connectUser = "jules", Pg.connectDatabase = "jules" }
    req <- parseUrl serverUrl
    withManager defaultManagerSettings $ \m ->
       withHTTP req m $ \resp ->
            runEffect $ for (processEvtStream conn (responseBody resp)) (liftIO.print)
