{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Money.Prediction where

import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time.Calendar (addGregorianMonthsClip, toGregorian, fromGregorian)

import           Money.DB.Types
import           Money.FilterSort
import           Money.Lens

import           Util (sha256)

import           Protolude
import           Prelude (String)

type DayOfMonth = Int
type Year = Integer
type Month = Int

data CCInfo =
    CCInfo {
        cycleStartDay :: DayOfMonth
      , cycleEndDay :: DayOfMonth
      , billDueDay :: DayOfMonth
      , ccAccId :: AccId
      , payFromAccId :: AccId
      , paymentTransactionName :: String
    } deriving (Show)

mkTxnId :: Txn -> Txn
mkTxnId x = x { txnId = (T.take 37 (toS (B64Url.encode (sha256 (show x :: String))))) <> "-p" }

{-
    The credit card bill that is due in April will include expense transactions
    from February to March.and refund transactions since the
    last payment in March. I.e. refunds do not follow the "cycle" and are "due"
    immediately. Furthermore, we don't want to include past credit card bill
    payments in the calculation of the bill.
-}
calculateCCBill :: CCInfo -> (Year, Month) -> DB -> [Txn]
calculateCCBill CCInfo{..} (year, month) db =
    let subtractMonths m = addGregorianMonthsClip (-m)

        billStart   = subtractMonths 2 $ fromGregorian year month cycleStartDay
        billEnd     = subtractMonths 1 $ fromGregorian year month cycleEndDay
        billDue     = fromGregorian year month billDueDay

        expenses = fDate ((>= billStart) ^& (<= billEnd)) ^& fAmount (> 0)
        refunds  = fDate ((>= subtractMonths 1 billDue) ^& (< billDue)) ^& fAmount (< 0)
        ff = ((== ccAccId) . accountId) ^& (not . fDesc paymentTransactionName) 
                                       ^& (expenses ^| refunds)
    in
        map mkTxnId
            [ Txn 
                (toS paymentTransactionName)
                (fromGregorian year month billDueDay)
                Nothing
                (negate $ sum $ map amount $ filter ff (txns db))
                Set.empty
                Nothing
                ccAccId
                ""
                Nothing
            , Txn
                (toS paymentTransactionName <> maybe "" (\x -> " " <> x) (fmap accNumber (Map.lookup ccAccId (accDB db))))
                (fromGregorian year month billDueDay)
                Nothing
                (sum $ map amount $ filter ff (txns db))
                Set.empty
                Nothing
                payFromAccId
                ""
                Nothing
            ]


predictedCCBills :: DB -> [CCInfo] -> [Txn]
predictedCCBills db =
    concatMap
        (\ccInfo@CCInfo{..} ->
            let
                mLastPaymentDate =
                    fmap date $ head $ sortBy sortDesc $
                        filter (((== ccAccId) . accountId) ^& fDesc paymentTransactionName) $ txns db
            in
                maybe
                    []
                    (\lastPaymentDate ->
                        if billDueDay <= ((\(_, _, d) -> d) $ toGregorian lastPaymentDate) then -- already payed bill this month
                            concat 
                                [ calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 1 lastPaymentDate) db
                                , calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 2 lastPaymentDate) db
                                ]
                        else
                            concat
                                [ calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian lastPaymentDate) db
                                , calculateCCBill ccInfo ((\(y, m, _) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 1 lastPaymentDate) db
                                ])
                    mLastPaymentDate)
