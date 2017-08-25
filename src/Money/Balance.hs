{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Money.Balance
    where

import qualified Data.Map as Map

import Money.DB.Types
import Money.FilterSort (sortDesc)
import Money.Lens (txns)

import Protolude

-- depository balance is balance in account, credit balance is total balance on credit card to be paid
-- therefore expenses in depository accounts are -ve and expenses in credit accounts are +ve
calculateTxnBalances :: DB -> Map TxnId Int
calculateTxnBalances db =
    mergeMaps $
        map
            (\(accId, acc) ->
                let
                    sortedTxns = sortDesc $ filter ((== accId) . accountId) $ txns db
                in
                    (\initBal ->
                        snd $ foldl'
                            (\(curBal, out) Txn{..} -> do
                                (let newBal = case accType acc of
                                        Depository -> curBal + amount
                                        Credit -> curBal - amount
                                 in (newBal, Map.insert txnId curBal out)))
                            (balAmount initBal, Map.empty)
                            (dropWhile (/= balTxn initBal) sortedTxns))
                    (balance acc))
            (Map.toList $ accDB db)
    where
        mergeMaps :: (Ord k) => [Map k a] -> Map k a
        mergeMaps = foldl' Map.union Map.empty
