{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Money.DB
    where

import           Control.Lens (over)
import           Data.Aeson (Value(..), object)
import qualified Data.Aeson as Aeson
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as BS
import           Data.List (foldl', groupBy, nub, sortBy, tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Time.Calendar (fromGregorian)
import           Data.SafeCopy (SafeCopy(..), Migrate(..), base, contain, deriveSafeCopySimple, extension, safeGet, safePut)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           Data.Text (Text)
import qualified Data.Text as T

import           Money.Plaid
import           Money.FilterSort (sortDesc, sortDesc_v1)
import           Money.DB.Types
import           Money.Lens

import           Prelude (FilePath, String)
import           Protolude hiding (maybeToEither)
import           Util (maybeToEither)

emptyDB :: DB
emptyDB = DB Map.empty Map.empty

mkAccDB :: [Account] -> AccDB
mkAccDB = Map.fromList . map (\acc -> (_id acc, acc))

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
        safePut plAmount
        safePut plDate
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
     
instance SafeCopy Txn_v1 where
     putCopy Txn_v1{..} = contain $ do
         safePut plaidTxn_v1
         safePut tags_v1
         safePut link_v1
         safePut betterDescription
         safePut txnBalance
     getCopy = contain $ Txn_v1 <$>
         safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy Txn where
    putCopy Txn{..} = contain $ do
        safePut desc
        safePut date
        safePut time
        safePut amount
        safePut tags
        safePut link
        safePut accountId
        safePut txnId
        safePut plaidTxn
    getCopy = contain $ Txn <$>
         safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*>
         safeGet <*> safeGet <*> safeGet <*> safeGet

deriveSafeCopySimple 1 'base ''AccType
deriveSafeCopySimple 1 'base ''Balance

instance SafeCopy Account where
    putCopy Account{..} = contain $ do
        safePut _id
        safePut plaidAccount
        safePut accType
        safePut accNumber
        safePut accName
        safePut balance
        safePut previousBalances
    getCopy = contain $ Account <$>
         safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

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
        safePut plBalance
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

instance SafeCopy DB_v1 where
    version = 2
    kind = extension
    putCopy DB_v1{..} = contain $ do safePut txnDB_v1; safePut accDB_v1;
    getCopy = contain $ DB_v1 <$> safeGet <*> safeGet

instance SafeCopy DB where
    version = 3
    kind = extension
    putCopy DB{..} = contain $ do safePut txnDB; safePut accDB;
    getCopy = contain $ DB <$> safeGet <*> safeGet

instance Migrate DB_v1 where
     type MigrateFrom DB_v1 = DB_v0
     migrate DB_v0{..} = DB_v1 (Map.map (\Txn_v0{..} -> Txn_v1 plaidTxn_v0 tags_v0 link_v0 Nothing 0) txnDB_v0) accDB_v0

instance Migrate DB where
    type MigrateFrom DB = DB_v1
    migrate DB_v1{..} =
        DB 
            newTxnDB
            (Map.map 
                (plAccToAcc newTxnDB)
                accDB_v1)
        where
            newTxnDB = Map.mapMaybe (plTxnToTxn . plaidTxn_v1) txnDB_v1

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
addTags = txnDBOp (\tagsToAdd -> tagsLens `over` (Set.union tagsToAdd))

removeTags :: [Txn] -> Set Tag -> DBOp
removeTags = txnDBOp (\tagsToRemove -> tagsLens `over` (`Set.difference` tagsToRemove))

plAccToAcc :: TxnDB -> PlaidAccount -> Account
plAccToAcc txnDb pa@PlaidAccount{..} =
    Account 
        acc_id
        (Just pa)
        (if acc_type == "credit" then Credit else Depository)
        (number acc_meta)
        (accMeta_name acc_meta)
        (maybe
            (Balance 0 (Txn "FAKE TXN NO BALANCE" (fromGregorian 2017 1 1) Nothing 0 Set.empty Nothing acc_id "txnidnomatch" Nothing))
            (Balance (floor ((current plBalance) * 100)))
            (latestTxn acc_id txnDb))
        []
    where
        latestTxn :: AccId -> TxnDB -> Maybe Txn
        latestTxn accId = head . sortDesc . filter ((== accId) . accountId) . map snd . Map.toList

plTxnToTxn :: PlaidTransaction -> Maybe Txn
plTxnToTxn pt@PlaidTransaction{..} = do
    dt <- (parseTime "%Y-%m-%d" . T.unpack $ plDate)
    return $ Txn
        txn_name
        dt
        Nothing
        (floor $ plAmount * 100)
        Set.empty
        Nothing
        _account
        txn_id
        (Just pt)

mergeAccDB :: Map AccId Account -> Map AccId Account -> Either String (Map AccId Account)
mergeAccDB old new = 
    if Set.fromList (Map.keys old) /= Set.fromList (Map.keys new) then
        Left "unknown new account in PlaidResponse"
    else
        Right $
            Map.unionWith
                (\a b -> b { previousBalances = balance a:previousBalances a })
                old
                new

mergeNewResponses :: [PlaidResponse] -> DB -> Either String DB
mergeNewResponses resps DB{..} = do
    txnDb <- foldl'
        (\eCurDb t -> do
            curDb <- eCurDb
            txn <- maybeToEither ("parseDate failed for " ++ show t) (plTxnToTxn t)
            maybe
                (return $ Map.insert (txn_id t) txn curDb)
                (\existingTxn ->
                    bool
                        (Left $ "txn mismatch: " ++ show existingTxn ++ " /= " ++ show t)
                        (return curDb)
                        (amount existingTxn == floor ((plAmount t) * 100) &&
                            ((==) `on` date) existingTxn txn))
                (Map.lookup (txn_id t) curDb))
        (Right txnDB)
        (concatMap transactions resps)
    accDb <- mergeAccDB accDB $ mkAccDB $ map (plAccToAcc txnDb) $ concatMap accounts resps
    return $ DB txnDb accDb
