module Money.DB.Types
    where

import Data.Text (Text)
import Data.CaseInsensitive (CI)
import Data.Set (Set)
import Data.Map (Map)

import Money.Plaid

type Tag = CI Text

data Txn_v0 = Txn_v0 { plaidTxn_v0 :: PlaidTransaction, tags_v0 :: Set Tag, link_v0 :: Maybe Text } deriving (Eq, Show)
data Txn = Txn {
    plaidTxn          :: PlaidTransaction
  , tags              :: Set Tag
  , link              :: Maybe Text
  , betterDescription :: Maybe Text
  , txnBalance        :: Double -- Balance for this account after transaction completed
  } deriving (Eq, Show)

type TxnId = Text
type AccId = Text
type TxnDB_v0 = Map TxnId Txn_v0
type TxnDB = Map TxnId Txn
type AccDB = Map AccId PlaidAccount
data DB_v0 = DB_v0 { txnDB_v0 :: TxnDB_v0, accDB_v0 :: AccDB }
data DB = DB { txnDB :: TxnDB, accDB :: AccDB }

type DBOp = DB -> DB
