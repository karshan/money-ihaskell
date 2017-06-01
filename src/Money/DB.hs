{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
module Money.DB
    where

import           Data.Aeson (Value(..), object)
import qualified Data.Aeson as Aeson
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Bool (bool)
import qualified Data.ByteString as BS
import           Data.List (foldl', groupBy, nub, sortBy, tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.SafeCopy (SafeCopy(..), Migrate(..), contain, extension, safeGet, safePut)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           Data.Text (Text)

import           Money.Plaid
import           Money.FilterSort (sortDesc)
import           Money.DB.Types
import           Money.Lens

emptyDB :: DB
emptyDB = DB Map.empty Map.empty

mkAccDB :: [PlaidAccount] -> AccDB
mkAccDB = Map.fromList . map (\acc -> (acc_id acc, acc))

instance (CI.FoldCase s, SafeCopy s) => SafeCopy (CI s) where
    putCopy ci = contain $ safePut (CI.foldedCase ci)
    getCopy = contain $ CI.mk <$> safeGet

instance SafeCopy Value where
    putCopy v = contain $ safePut (Aeson.encode v)
    getCopy = contain $ fromMaybe (object []) . Aeson.decode <$> safeGet

instance SafeCopy PlaidTransactionMeta where
    putCopy PlaidTransactionMeta{..} = contain $ safePut txnMeta_location
    getCopy = contain $ PlaidTransactionMeta <$> safeGet

instance SafeCopy PlaidTransactionType where
    putCopy PlaidTransactionType{..} = contain $ safePut primary
    getCopy = contain $ PlaidTransactionType <$> safeGet

instance SafeCopy PlaidTransactionScore where
    putCopy PlaidTransactionScore{..} = contain $ do 
        safePut txnScore_location
        safePut txnScore_name
    getCopy = contain $ PlaidTransactionScore <$> safeGet <*> safeGet

instance SafeCopy PlaidTransaction where
    putCopy PlaidTransaction{..} = contain $ do
        safePut _account
        safePut _pendingTransaction
        safePut txn_id
        safePut amount
        safePut date                
        safePut txn_name            
        safePut txn_meta            
        safePut pending             
        safePut txn_type            
        safePut category            
        safePut category_id         
        safePut score
    getCopy = contain $ PlaidTransaction <$> 
        safeGet <*> safeGet <*> safeGet <*> safeGet <*>
        safeGet <*> safeGet <*> safeGet <*> safeGet <*>
        safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy Txn_v0 where
     putCopy Txn_v0{..} = contain $ do safePut plaidTxn_v0; safePut tags_v0; safePut link_v0
     getCopy = contain $ Txn_v0 <$> safeGet <*> safeGet <*> safeGet
     
instance SafeCopy Txn where
     putCopy Txn{..} = contain $ do
         safePut plaidTxn
         safePut tags
         safePut link
         safePut betterDescription
         safePut txnBalance
     getCopy = contain $ Txn <$>
         safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy PlaidBalance where
    putCopy PlaidBalance{..} = contain $ do safePut available; safePut current
    getCopy = contain $ PlaidBalance <$> safeGet <*> safeGet

instance SafeCopy PlaidAccountMeta where
    putCopy PlaidAccountMeta{..} = contain $ do
        safePut limit
        safePut accMeta_name
        safePut number
        safePut official_name
    getCopy = contain $ PlaidAccountMeta <$>
        safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy PlaidAccount where
    putCopy PlaidAccount{..} = contain $ do
        safePut acc_id
        safePut _item
        safePut _user
        safePut balance
        safePut institution_type
        safePut acc_meta
        safePut subtype
        safePut acc_type
    getCopy = contain $ PlaidAccount <$>
        safeGet <*> safeGet <*> safeGet <*> safeGet <*>
        safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy DB_v0 where
    putCopy DB_v0{..} = contain $ do safePut txnDB_v0; safePut accDB_v0;
    getCopy = contain $ DB_v0 <$> safeGet <*> safeGet

instance SafeCopy DB where
    version = 2
    kind = extension
    putCopy DB{..} = contain $ do safePut txnDB; safePut accDB;
    getCopy = contain $ DB <$> safeGet <*> safeGet

instance Migrate DB where
     type MigrateFrom DB = DB_v0
     migrate DB_v0{..} = calculateTxnBalances $ DB (Map.map (\Txn_v0{..} -> Txn plaidTxn_v0 tags_v0 link_v0 Nothing 0) txnDB_v0) accDB_v0

-- This function also deletes all transactions in the db that map to a non-existing account
-- i.e. 
--    if Map.lookup (_account . plaidTxn $ t) (accDB db) == Nothing
--    then t is deleted
calculateTxnBalances :: DB -> DB
calculateTxnBalances db =
    let outTxns =
            concatMap
                (\(accId, acc) ->
                    let
                        sortedTxns = sortDesc $ filter ((== accId) . _account . plaidTxn) $ txns db
                    in
                        snd $ foldl'
                            (\(curBal, out) Txn{..} ->
                                ((case acc_type acc of -- depository balance is balance in account, credit balance is total balance on credit card to be paid
                                    "depository" -> curBal + amount plaidTxn
                                    "credit" -> curBal - amount plaidTxn
                                    otherwise -> error "unknown account type"), (Txn plaidTxn tags link betterDescription curBal):out)) -- TODO use `over` here
                            (current $ balance acc, [])
                            sortedTxns)
                (Map.toList (accDB db))
    in DB (Map.fromList $ map (\t -> (txnId t, t)) outTxns) (accDB db)

serializeDB :: FilePath -> DB -> IO ()
serializeDB fn = BS.writeFile fn . runPut . safePut

deserializeDB :: FilePath -> IO (Either String DB)
deserializeDB fn = runGet safeGet <$> BS.readFile fn

txnDBOp :: (a -> Txn -> Txn) -> [Txn] -> a -> DBOp
txnDBOp f ts a DB{..} =
    DB
        (Set.foldr
            (Map.update (Just . f a))
            txnDB
            (Set.fromList $ map txnId ts))
        accDB

addTags :: [Txn] -> Set Tag -> DBOp
addTags = txnDBOp (\tagsToAdd Txn{..} -> Txn plaidTxn (Set.union tagsToAdd tags) link betterDescription txnBalance)

removeTags :: [Txn] -> Set Tag -> DBOp
removeTags = txnDBOp (\tagsToRemove Txn{..} -> Txn plaidTxn (tags `Set.difference` tagsToRemove) link betterDescription txnBalance)

mergeNewResponses :: [PlaidResponse] -> DBOp
mergeNewResponses resps DB{..} =
    calculateTxnBalances $ DB 
        (foldl'
            (\curDb t -> 
                maybe
                    (Map.insert (txn_id t) (Txn t Set.empty Nothing Nothing 0) curDb)
                    (\existingTxn ->
                        bool
                            (error $ "txn amount mismatch: " ++ show existingTxn ++ " /= " ++ show t)
                            curDb
                            (amount (plaidTxn existingTxn) == amount t))
                    (Map.lookup (txn_id t) curDb))
            txnDB
            (concatMap transactions resps))
        (mkAccDB $ concatMap accounts resps)
