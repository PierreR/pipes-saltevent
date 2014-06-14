{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main
where

import SaltEvent
import           Control.Lens                       (view)
import           Data.ByteString                    (ByteString)
import qualified Database.PostgreSQL.Simple         as Pg
import           Pipes
import qualified Pipes.Aeson                        as PAe
import qualified Pipes.ByteString                   as PB
import           Pipes.Group
import           Pipes.HTTP
import           Pipes.Parse

-- Removing "data: " by brute force for now
jsonLowerBound :: Int
jsonLowerBound = 6

token = "edf5e44d8c12852af87d62423c21bf7f"
serverUrl = "http://localhost:8080/event/" ++ token ++ "?tag=salt%2Fjob%2F"

insertSQL = "insert into event (jid, username, stamp, tgt, minions, fun, arg) values (?, ?, ?, ?, ?, ?, ?)"

-- lens getters are functions of Producers
getLines::
    Monad m
    => Producer ByteString m ()
    -> FreeT (Producer ByteString m) m ()
getLines = view PB.lines

processEvtStream :: MonadIO m => Pg.Connection -> Producer ByteString m () -> Producer SaltEvent m ()
processEvtStream conn = go . getLines
  where
    go :: MonadIO m => FreeT (Producer ByteString m) m r -> Producer SaltEvent m r
    go freeT = do
        x <- lift $ runFreeT freeT -- runFreeT to get the next line (the next Producer inside the FreeT)
        case x of
            Pure r -> -- There aren't no group left for instance when the server stops streaming events
                liftIO $ return r
            Free p -> do
                (jr, p') <- lift $ runStateT PAe.decode (p >-> PB.drop jsonLowerBound)
                case jr of
                    Just(Right jv) -> do
                        let cmd = _data jv
                        liftIO $ print cmd
                        liftIO $ Pg.execute conn insertSQL cmd
                        yield jv
                    _  -> return () -- json parser returns an error
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
