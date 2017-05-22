{-# LANGUAGE OverloadedStrings #-}
module Money.Lens
    where

import           Data.Time (Day, UTCTime (..))
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

import           Money.Plaid
import           Money.DB.Types

parseTime :: String -> String -> Maybe Day
parseTime f = fmap utctDay . parseTimeM True defaultTimeLocale f

-- pseudo lens
txns :: DB -> [Txn]
txns = map snd . Map.toList . txnDB

accs :: DB -> [PlaidAccount]
accs = map snd . Map.toList . accDB

date' :: Txn -> Day
date' = fromMaybe (error "parseTime failed") . parseTime "%Y-%m-%d" . T.unpack . date . plaidTxn

-- psuedoLenses
acc :: AccId -> DB -> PlaidAccount
acc accId = fromMaybe (error "invalid accId") . Map.lookup accId . accDB

accNumber :: AccId -> DB -> Text
accNumber accId = number . acc_meta . acc accId 

accBalance :: AccId -> DB -> Double
accBalance accId = current . balance . acc accId

accName :: AccId -> DB -> Text
accName accId = (\x -> accMeta_name x <> " " <> number x) . acc_meta . acc accId

txnId :: Txn -> TxnId
txnId = txn_id . plaidTxn
