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
--import Database.PostgreSQL.Simple
import           GHC.Generics
import           Pipes
import qualified Pipes.Aeson         as PAe
import qualified Pipes.ByteString    as PB
import           Pipes.Group
import           Pipes.HTTP
import           Pipes.Parse

-- Removing "data: " by brute force for now
jsonLowerBound :: Int
jsonLowerBound = 6

-- JSON PARSING
data Event = Event {
    tag     :: String
    , _data :: Command
} deriving Show

data Command = Command {
    jid       :: String
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

-- lens getters are functions of Producers
getLines::
    Monad m
    => Producer ByteString m ()
    -> FreeT (Producer ByteString m) m ()
getLines = view PB.lines

processEvtStream :: MonadIO m => Producer ByteString m () -> Producer Event m ()
processEvtStream = go . getLines
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
                    Right jv -> yield jv
                -- p' :: Producer ByteString m (FreeT (Producer ByteString m) m r)
                freeT' <- lift $ drain p'
                go freeT'

    drain :: Monad m => Producer a m r -> m r
    drain p = runEffect $ for p discard

main = do
    req <- parseUrl "http://localhost:8080/event/2b230e965c65eaf992962be7f0b0ab9b?tag=salt%2Fjob%2F"
    withManager defaultManagerSettings $ \m ->
       withHTTP req m $ \resp ->
            runEffect $ for (processEvtStream (responseBody resp)) (liftIO.print)
