{-# LANGUAGE NoImplicitPrelude #-}
module MoneySyncService.Client where

import MoneySyncService.API
import MoneySyncService.Types
import Servant.Client
import Servant.API
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Protolude

dbGet :: ClientM GetDBResponse
institutionCreate :: CreateInstitution -> ClientM ()
errorlogGet :: ClientM [Text]
errorlogClear :: ClientM ()
dbGet :<|> institutionCreate :<|> errorlogGet :<|> errorlogClear = client (Proxy :: Proxy API)

run :: MonadIO m => ClientM resp -> m (Either ServantError resp)
run rpc = liftIO $ do
    mgr <- newManager (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro (120 * 10^6) })
    runClientM rpc
        (ClientEnv mgr
            (BaseUrl Http "localhost" 10001 ""))
