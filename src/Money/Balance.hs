{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Money.Balance
    where

import qualified Data.Map as Map

import Money.DB.Types
import Money.FilterSort (sortDesc)
import Money.Lens (txns)

import Protolude hiding (link)

-- balance in depository accounts represents how much money is in the account (balance = +$100 = $100 in account)
-- balance in credit accounts represents how much debt is owed. (balance = +$100 = $100 owed)
-- expenses in both accounts types are represented by +ve amounts
calculateTxnBalances :: DB -> Map TxnId Int
calculateTxnBalances db =
    mergeMaps $
        map
            (\(accId, acc) ->
                let sortedTxns = sortBy sortDesc $ filter ((== accId) . accountId) $ txns db
                    initBal = balance acc
                    txnsAfterBal = takeWhile (/= balTxn initBal) sortedTxns
                    txnsBeforeBal = dropWhile (/= balTxn initBal) sortedTxns
                in
                    Map.union
                        (snd $ foldl'
                            (\(curBal, out) Txn{..} -> do
                                (let newBal = case accType acc of
                                        Depository -> curBal + amount
                                        Credit -> curBal - amount
                                 in (newBal, Map.insert txnId curBal out)))
                            (balAmount initBal, Map.empty)
                            txnsBeforeBal)
                        (snd $ foldl'
                            (\(curBal, out) Txn{..} -> do
                                (let newBal = case accType acc of
                                        Depository -> curBal - amount
                                        Credit -> curBal + amount
                                 in (newBal, Map.insert txnId newBal out)))
                            (balAmount initBal, Map.empty)
                            (reverse txnsAfterBal)))
            (Map.toList $ accDB db)
    where
        mergeMaps :: (Ord k) => [Map k a] -> Map k a
        mergeMaps = foldl' Map.union Map.empty
