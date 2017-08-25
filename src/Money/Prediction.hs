{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Money.Prediction where

import           Data.Text (Text)
import qualified Data.Text as T (pack)
import           Data.Time.Calendar (Day, addDays, addGregorianMonthsClip, diffDays, toGregorian, fromGregorian)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

import           Money.DB.Types
import           Money.FilterSort
import           Money.Lens
import           Money.Plaid

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
      , accountNumber :: Text
      , paymentTransactionName :: String
    } deriving (Show)

data PredictedTxn =
    PredictedTxn {
        prdAmount :: Double
      , prdName :: Text
      , prdDate :: Day
      , prdAcc :: Text
    } deriving (Show)

{-
    The credit card bill that is due in April will include expense transactions
    from February to March.and refund transactions since the
    last payment in March. I.e. refunds do not follow the "cycle" and are "due"
    immediately. Furthermore, we don't want to include past credit card bill
    payments in the calculation of the bill.
-}
calculateCCBill :: CCInfo -> (Year, Month) -> DB -> PredictedTxn
calculateCCBill CCInfo{..} (year, month) db =
    let subtractMonths m = addGregorianMonthsClip (-m)

        billStart   = subtractMonths 2 $ fromGregorian year month cycleStartDay
        billEnd     = subtractMonths 1 $ fromGregorian year month cycleEndDay
        billDue     = fromGregorian year month billDueDay

        expenses = fDate ((>= billStart) ^& (<= billEnd)) ^& fAmount (> 0)
        refunds  = fDate ((>= subtractMonths 1 billDue) ^& (< billDue)) ^& fAmount (< 0)
        ff = fAccount db accountNumber ^& (not . fDesc paymentTransactionName) 
                                       ^& (expenses ^| refunds)
    in
        PredictedTxn 
            (sum $ map amount' $ filter ff (txns db))
            (T.pack paymentTransactionName)
            (fromGregorian year month billDueDay)
            accountNumber

predictedCCBills :: DB -> [CCInfo] -> [PredictedTxn]
predictedCCBills db =
    concatMap
        (\ccInfo@CCInfo{..} ->
            let
                mLastPaymentDate =
                    fmap date $ head $ sortDesc $
                        filter (fAccount db accountNumber ^& fDesc paymentTransactionName) $ txns db
            in
                maybe
                    []
                    (\lastPaymentDate ->
                        if billDueDay <= ((\(y, m, d) -> d) $ toGregorian lastPaymentDate) then -- already payed bill this month
                            [ calculateCCBill ccInfo ((\(y, m, d) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 1 lastPaymentDate) db
                            , calculateCCBill ccInfo ((\(y, m, d) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 2 lastPaymentDate) db
                            ]
                        else
                            [ calculateCCBill ccInfo ((\(y, m, d) -> (y, m)) $ toGregorian lastPaymentDate) db
                            , calculateCCBill ccInfo ((\(y, m, d) -> (y, m)) $ toGregorian $ addGregorianMonthsClip 1 lastPaymentDate) db
                            ])
                    mLastPaymentDate)
