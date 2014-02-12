{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, DeriveGeneric #-}
module Main
where

import Control.Applicative
import Control.Monad
import Control.Lens(view)
import Data.Aeson
import Data.ByteString (ByteString)
--import Database.PostgreSQL.Simple
import GHC.Generics
import Pipes
import qualified Pipes.Aeson as PAe
import Pipes.HTTP
import Pipes.Group
import Pipes.Parse
import qualified Pipes.ByteString as PB

data Event = Event {
    tag :: String
    , _data :: Command
} deriving Show

data Command = Command {
    jid :: String
    , user :: String
    , _stamp :: String
    , tgt :: String
    , minions :: [String]
    , fun :: String
    , arg :: [Value]
} deriving (Show, Generic)

instance FromJSON Command

instance FromJSON Event where
     parseJSON (Object v) = Event <$>
                            v .: "tag" <*>
                            v .: "data"
     parseJSON _          = mzero

sample = "{\"tag\": \"salt/job/20140203110645016162/new\", \"data\": {\"tgt_type\": \"glob\", \"jid\": \"20140203110645016162\", \"tgt\": \"salt\", \"_stamp\": \"2014-02-03_11:06:45.016495\", \"user\": \"vagrant\", \"arg\": [{\"module\": \"t\", \"__kwarg__\": true}], \"fun\": \"sys.argspec\", \"minions\": [\"salt\"]}}"

-- Removing "data: " by brute force for now
jsonLowerBound :: Int
jsonLowerBound = 6

filters :: (MonadIO m) => Producer ByteString m () -> Producer Event m ()
filters = loop . view PB.lines
  where
    loop :: MonadIO m => FreeT (Producer ByteString m) m r -> Producer Event m r
    loop freeT = do
        x <- lift $ runFreeT freeT
        case x of
            Pure r -> return r
            Free p -> do
                (j, p') <- lift $ runStateT PAe.decode (p >-> PB.drop jsonLowerBound)
                case j of
                    Left  _    -> return () -- or deal with the error liftIO $ print e
                    Right j' -> yield j'
                freeT' <- lift $ drain p'
                loop freeT'

    drain :: Monad m => Producer a m r -> m r
    drain p = runEffect $ for p discard

main = do
    req <- parseUrl "http://localhost:8080/event/2b230e965c65eaf992962be7f0b0ab9b?tag=salt%2Fjob%2F"
    withManager defaultManagerSettings $ \m ->
       withHTTP req m $ \resp ->
            runEffect $ for (filters (responseBody resp)) (liftIO.print)