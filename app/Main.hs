{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Money.DB
import Money.DB.Types
import Money.Prediction
import Money.Lens
import Money.Plaid
import Money.FilterSort

showTxn t = (date $ plaidTxn t, txn_name $ plaidTxn t, amount $ plaidTxn t)

bofaInfo     = CCInfo 20 19 14 "0881" "PAYMENT - THANK YOU"
amazonInfo   = CCInfo 07 06 03 "4231" "AUTOMATIC PAYMENT - THANK"
unitedInfo   = CCInfo 13 12 09 "8421" "AUTOMATIC PAYMENT - THANK"
sapphireInfo = CCInfo 25 24 21 "4569" "AUTOMATIC PAYMENT - THANK"

ccInfos = [bofaInfo, amazonInfo, unitedInfo, sapphireInfo]

main :: IO ()
main = do
    Right db <- deserializeDB "latest.db"
    mapM_ print $ predictedCCBills db ccInfos
