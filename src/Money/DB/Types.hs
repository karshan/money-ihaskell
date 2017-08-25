{-# LANGUAGE RecordWildCards #-}
module Money.DB.Types
    where

import Control.Lens (Lens')
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime)
import Data.CaseInsensitive (CI)
import Data.Set (Set)
import Data.Map (Map)

import Money.Plaid

type Tag = CI Text

data Txn_v0 = Txn_v0 { plaidTxn_v0 :: PlaidTransaction, tags_v0 :: Set Tag, link_v0 :: Maybe Text } deriving (Eq, Show)
data Txn_v1 = Txn_v1 {
    plaidTxn_v1       :: PlaidTransaction
  , tags_v1           :: Set Tag
  , link_v1           :: Maybe Text
  , betterDescription :: Maybe Text
  , txnBalance        :: Double -- Balance for this account after transaction completed
  } deriving (Eq, Show)

data Txn = Txn {
    desc      :: Text
  , date      :: Day
  , time      :: Maybe DiffTime
  , amount    :: Int
  , tags      :: Set Tag
  , link      :: Maybe Text
  , accountId :: Text 
  , txnId     :: Text 
  , plaidTxn  :: Maybe PlaidTransaction
  } deriving (Eq, Show)

tagsLens :: Lens' Txn (Set Tag)
tagsLens f t@Txn{..} = fmap (\tags' -> t { tags = tags' }) (f tags)

data AccType = Depository | Credit deriving (Eq, Ord, Show)

data Balance = Balance {
    balAmount  :: Int
  , balTxn     :: Txn
  } deriving (Eq, Show)

data Account = Account {
    _id              :: Text
  , plaidAccount     :: Maybe PlaidAccount
  , accType          :: AccType
  , accNumber        :: Text
  , accName          :: Text
  , balance          :: Balance
  , previousBalances :: [Balance]
  } deriving (Eq, Show)

type TxnId = Text
type AccId = Text
type TxnDB_v0 = Map TxnId Txn_v0
type TxnDB_v1 = Map TxnId Txn_v1
type TxnDB = Map TxnId Txn
type AccDB_v0 = Map AccId PlaidAccount
type AccDB = Map AccId Account
data DB_v0 = DB_v0 { txnDB_v0 :: TxnDB_v0, accDB_v0 :: AccDB_v0 }
data DB_v1 = DB_v1 { txnDB_v1 :: TxnDB_v1, accDB_v1 :: AccDB_v0 }
data DB = DB { txnDB :: TxnDB, accDB :: AccDB }

type DBOp = DB -> DB
