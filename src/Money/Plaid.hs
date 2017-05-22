{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
module Money.Plaid
    ( Creds(..)
    , PlaidAccount(..)
    , PlaidAccountMeta(..)
    , PlaidBalance(..)
    , PlaidTransaction(..)
    , PlaidTransactionMeta(..)
    , PlaidTransactionScore(..)
    , PlaidTransactionType(..)
    , PlaidResponse(..)
    , plaidGet
    )
    where

import           Data.Aeson ((.=), (.:), (.:?), (.!=), FromJSON(..), ToJSON(..), Value(..), object)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)

import           GHC.Generics (Generic)

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Servant.API ((:>), ReqBody, Post, JSON, FormUrlEncoded)
import           Servant.Client (BaseUrl(..), ClientEnv(..), ClientM, Scheme(Https), ServantError, client, runClientM)

import           Web.FormUrlEncoded (ToForm(..))

-- Plaid Credentials
data Creds =
    Creds {
        cr_clientId      :: Text
      , cr_clientSecret  :: Text
      , accessTokens     :: [Text]
    } deriving (Eq, Show)

data PlaidRequestBody =
    PlaidRequestBody {
        req_clientId     :: Text
      , req_clientSecret :: Text
      , accessToken      :: Text
      , options          :: Text
    } deriving (Eq, Show)

instance ToJSON PlaidRequestBody where
    toJSON PlaidRequestBody{..} =
        object
            [ "client_id"     .= req_clientId
            , "client_secret" .= req_clientSecret
            , "access_token"  .= accessToken
            , "options"       .= options
            ]

instance ToForm PlaidRequestBody where
    toForm PlaidRequestBody{..} =
        toForm
            ([ ("client_id", req_clientId)
            , ("secret", req_clientSecret)
            , ("access_token", accessToken)
            , ("options", options)
            ] :: [(Text, Text)])

data PlaidResponse =
    PlaidResponse {
        accounts     :: [PlaidAccount]
      , transactions :: [PlaidTransaction]
      , resp_accessToken :: Text
    } deriving (Eq, Show, Read)

instance FromJSON PlaidResponse where
  parseJSON (Object v) =
      PlaidResponse <$>
          v .: "accounts"     <*>
          v .: "transactions" <*>
          v .: "access_token"

data PlaidAccount =
    PlaidAccount {
        acc_id           :: Text
      , _item            :: Text
      , _user            :: Text
      , balance          :: PlaidBalance
      , institution_type :: Text
      , acc_meta         :: PlaidAccountMeta
      , subtype          :: Text
      , acc_type         :: Text
    } deriving (Eq, Show, Read)

instance FromJSON PlaidAccount where
  parseJSON (Object v) =
      PlaidAccount <$>
          v .: "_id"              <*>
          v .: "_item"            <*>
          v .: "_user"            <*>
          v .: "balance"          <*>
          v .: "institution_type" <*>
          v .: "meta"             <*>
          v .: "subtype"          <*>
          v .: "type"

data PlaidAccountMeta =
    PlaidAccountMeta {
        limit         :: Maybe Double
      , accMeta_name  :: Text
      , number        :: Text
      , official_name :: Maybe Text
    } deriving (Eq, Show, Read)

instance FromJSON PlaidAccountMeta where
  parseJSON (Object v) =
      PlaidAccountMeta <$>
          v .: "limit"  <*>
          v .: "name"   <*>
          v .: "number" <*>
          v .: "official_name"

data PlaidBalance =
    PlaidBalance {
        available :: Maybe Double
      , current   :: Double
    } deriving (Generic, Eq, Show, Read)
instance FromJSON PlaidBalance

data PlaidTransaction =
    PlaidTransaction {
        _account            :: Text
      , _pendingTransaction :: Maybe Text
      , txn_id              :: Text
      , amount              :: Double
      , date                :: Text
      , txn_name            :: Text
      , txn_meta            :: PlaidTransactionMeta
      , pending             :: Bool
      , txn_type            :: PlaidTransactionType
      , category            :: Maybe [Text]
      , category_id         :: Maybe Text
      , score               :: PlaidTransactionScore
    } deriving (Eq, Show, Read)

instance FromJSON PlaidTransaction where
  parseJSON (Object v) =
      PlaidTransaction <$>
          v .: "_account"                         <*>
          v .:? "_pendingTransaction" .!= Nothing <*>
          v .: "_id"                              <*>
          v .: "amount"                           <*>
          v .: "date"                             <*>
          v .: "name"                             <*>
          v .: "meta"                             <*>
          v .: "pending"                          <*>
          v .: "type"                             <*>
          v .:? "category"    .!= Nothing         <*>
          v .:? "category_id" .!= Nothing         <*>
          v .: "score"

data PlaidTransactionMeta =
    PlaidTransactionMeta {
        txnMeta_location :: Value
    } deriving (Eq, Show, Read)

instance FromJSON PlaidTransactionMeta where
    parseJSON (Object v) =
        PlaidTransactionMeta <$>
            v .: "location"

data PlaidTransactionType =
    PlaidTransactionType {
        primary :: Text
    } deriving (Generic, Eq, Show, Read)
instance FromJSON PlaidTransactionType

data PlaidTransactionScore =
    PlaidTransactionScore {
        txnScore_location :: Value
      , txnScore_name     :: Double
    } deriving (Eq, Show, Read)

instance FromJSON PlaidTransactionScore where
    parseJSON (Object v) =
        PlaidTransactionScore <$>
            v .: "location" <*>
            v .: "name"

type PlaidAPI =
    "connect" :> "get" :> ReqBody '[FormUrlEncoded] PlaidRequestBody
                       :> Post '[JSON] PlaidResponse

plaidGet' :: PlaidRequestBody -> ClientM PlaidResponse
plaidGet' = client (Proxy :: Proxy PlaidAPI)

plaidGet :: Creds -> IO [Either ServantError PlaidResponse]
plaidGet Creds{..} = do
    mgr <- newManager tlsManagerSettings
    sequence $ map
        (\accessToken ->
            runClientM 
                (plaidGet'
                    (PlaidRequestBody
                        { req_clientId = cr_clientId
                        , req_clientSecret = cr_clientSecret
                        , accessToken = accessToken
                        , options = ""
                        }))
                (ClientEnv mgr
                    (BaseUrl Https "tartan.plaid.com" 443 "")))
        accessTokens


