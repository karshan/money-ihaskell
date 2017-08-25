{-# LANGUAGE OverloadedStrings #-}
module Money.Lens
    where

import           Data.Time (Day, UTCTime (..))
import           Data.Time.Format (defaultTimeLocale, parseTimeM)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Map as Map
import           Data.Monoid ((<>))

import           Money.Plaid
import           Money.DB.Types

parseTime :: String -> String -> Maybe Day
parseTime f = fmap utctDay . parseTimeM True defaultTimeLocale f

date_v1 :: Txn_v1 -> Maybe Day
date_v1 = parseTime "%Y-%m-%d" . T.unpack . plDate . plaidTxn_v1

-- pseudo lens
txns :: DB -> [Txn]
txns = map snd . Map.toList . txnDB

accs :: DB -> [Account]
accs = map snd . Map.toList . accDB

amount' :: Txn -> Double
amount' = (/100) . fromIntegral . amount

-- psuedoLenses
acc :: AccId -> DB -> Maybe Account
acc accId = Map.lookup accId . accDB

accNumber' :: AccId -> DB -> Maybe Text
accNumber' accId db = accNumber <$> acc accId db

accBalance :: AccId -> DB -> Maybe Balance
accBalance accId db = balance <$> acc accId db

accName' :: AccId -> DB -> Maybe Text
accName' accId db = (\x -> accName x <> " " <> accNumber x) <$> acc accId db
