{-# LANGUAGE NoImplicitPrelude #-}
module Money.Balance where

import MoneySyncService.Types
import Protolude
import qualified Data.Map as Map
import qualified Lenses as L
import Control.Lens
import Money.FilterSort

calculateTxnBalances :: Map AccountId Account -> Map TxnId Txn -> Map TxnId Int
calculateTxnBalances accMap txnMap =
    mergeMaps $
        map
            (\(accId, acc) ->
                let sortedTxns = sortBy sortDesc $ filter ((== accId) . view L.accountId) $ Map.elems txnMap
                    initBal = acc ^. L.balance ^. L.amount
                in
                    (snd $ foldl'
                        (\(curBal, out) txn -> do
                             (curBal - txn ^. L.amount, Map.insert (txn ^. L.id) curBal out))
                        (initBal, Map.empty)
                        sortedTxns))
                        {-(snd $ foldl'
                            (\(curBal, out) txn -> do
                                (let newBal = case acc ^. L._type of
                                        Debit -> curBal - amount
                                        Credit -> curBal + amount
                                 in (newBal, Map.insert txnId newBal out)))
                            (balAmount initBal, Map.empty)
                            (reverse txnsAfterBal)))-}
            (Map.toList accMap)
    where
        mergeMaps :: (Ord k) => [Map k a] -> Map k a
        mergeMaps = foldl' Map.union Map.empty
