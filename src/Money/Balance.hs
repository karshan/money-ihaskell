{-# LANGUAGE NoImplicitPrelude #-}
module Money.Balance where

import MoneySyncService.Types
import Protolude
import qualified Data.Map as Map
import qualified Lenses as L
import Control.Lens
import Money.FilterSort

calculateBalances :: Map AccountId Account -> Map TxnId Txn -> Map TxnId Txn -> Map TxnId Int
calculateBalances accMap txnMap fcstMap =
    mergeMaps $
        map
            (\(accId, acc) ->
                let sortedTxns = sortBy sortDesc $ filter ((== accId) . view L.accountId) $ Map.elems txnMap
                    sortedFcst = sortBy sortAsc $ filter ((== accId) . view L.accountId) $ Map.elems fcstMap
                    initBal = acc ^. L.balance ^. L.amount
                in
                    Map.union
                        (snd $ foldl'
                            (\(curBal, out) txn -> do
                                 (curBal - txn ^. L.amount, Map.insert (txn ^. L.id) curBal out))
                            (initBal, Map.empty)
                            sortedTxns)
                        (snd $ foldl'
                            (\(curBal, out) txn -> do
                                 let bal = curBal + txn ^. L.amount
                                 (bal, Map.insert (txn ^. L.id) bal out))
                            (initBal, Map.empty)
                            sortedFcst))
            (Map.toList accMap)
    where
        mergeMaps :: (Ord k) => [Map k a] -> Map k a
        mergeMaps = foldl' Map.union Map.empty
