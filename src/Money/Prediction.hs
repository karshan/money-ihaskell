{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Money.Prediction where

import qualified Data.Map as Map
import           Data.Time.Calendar (addGregorianMonthsClip, toGregorian, fromGregorian)

import           MoneySyncService.Types
import           Money.FilterSort
import qualified Lenses as L

import           Protolude
import           Prelude (String)
import Control.Lens

type DayOfMonth = Int
type Year = Integer
type Month = Int

data CCInfo =
    CCInfo {
        cycleStartDay :: DayOfMonth
      , cycleEndDay :: DayOfMonth
      , billDueDay :: DayOfMonth
      , ccAccId :: AccountId
      , payFromAccId :: AccountId
      , paymentTransactionName :: String
    } deriving (Show)

{-
    The credit card bill that is due in April will include expense transactions
    from February to March.and refund transactions since the
    last payment in March. I.e. refunds do not follow the "cycle" and are "due"
    immediately. Furthermore, we don't want to include past credit card bill
    payments in the calculation of the bill.
-}
calculateCCBill :: CCInfo -> (Year, Month) -> Map AccountId Account -> Map TxnId Txn -> [Txn]
calculateCCBill CCInfo{..} (year, month) accMap txnMap =
    let subtractMonths m = addGregorianMonthsClip (-m)

        billStart   = subtractMonths 2 $ fromGregorian year month cycleStartDay
        billEnd     = subtractMonths 1 $ fromGregorian year month cycleEndDay
        billDue     = fromGregorian year month billDueDay

        expenses = fDate ((>= billStart) ^& (<= billEnd)) ^& fAmount (> 0)
        refunds  = fDate ((>= subtractMonths 1 billDue) ^& (< billDue)) ^& fAmount (< 0)
        ff = ((== ccAccId) . view L.accountId) ^& (not . fDesc paymentTransactionName) 
                                       ^& (expenses ^| refunds)
        amt = sum $ map (view L.amount) $ filter ff (Map.elems txnMap)
    in
        [ emptyTxn &
            L.name .~ (toS paymentTransactionName) &
            L.date .~ (fromGregorian year month billDueDay) &
            L.amount .~ (negate amt) &
            L.accountId .~ ccAccId
        , emptyTxn &
            L.name .~ (toS paymentTransactionName <> maybe "" (\x -> " " <> x) (fmap (view L.number) (Map.lookup ccAccId accMap))) &
            L.date .~ (fromGregorian year month billDueDay) &
            L.amount .~ amt &
            L.accountId .~ payFromAccId
        ]


predictedCCBills :: Map AccountId Account -> Map TxnId Txn -> [CCInfo] -> [Txn]
predictedCCBills accMap txnMap =
    concatMap
        (\ccInfo@CCInfo{..} ->
            let
                mLastPaymentDate =
                    fmap (view L.date) $ head $ sortBy sortDesc $
                        filter (((== ccAccId) . view L.accountId) ^& fDesc paymentTransactionName) $ (Map.elems txnMap)
            in
                maybe
                    []
                    (\lastPaymentDate ->
                        if billDueDay <= ((\(_, _, d) -> d) $ toGregorian lastPaymentDate) then -- already payed bill this month
                            concat 
                                [ calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 1 lastPaymentDate) accMap txnMap
                                , calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 2 lastPaymentDate) accMap txnMap
                                ]
                        else
                            concat
                                [ calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian lastPaymentDate) accMap txnMap
                                , calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 1 lastPaymentDate) accMap txnMap
                                ])
                    mLastPaymentDate)
