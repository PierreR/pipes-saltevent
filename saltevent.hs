{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Control.Monad
import Control.Applicative
import Database.PostgreSQL.Simple
import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import Network.Connection (TLSSettings (..))


main = do
	--c <- connect defaultConnectInfo {
	--	connectDatabase = "jules"
	--}
	--mapM_ print =<< ( query_ c "select 2 + 2" :: IO [Only Int] )
	req <- parseUrl "https://127.0.0.1/event/0a6604ef57b6913734f2c6d172903296?tag=salt%2Fjob%2F"
	let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
	withManager settings $ \m ->
	   withHTTP req m $ \resp ->
	       runEffect $ (responseBody resp) >-> P.chain(\_ -> liftIO $ putStrLn "*********") >-> PB.stdout

