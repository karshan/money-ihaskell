{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Money.IHaskell where

import           Numeric (showFFloat)
import           Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.String (String, fromString)
import qualified Data.Set as Set

import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Lenses as L
import MoneySyncService.Types
import Money.FilterSort (FilterFunc, SortFunc)
import Data.Map (Map)
import Control.Lens
import Protolude

show2decimal :: Double -> String
show2decimal x = showFFloat (Just 2) x ""

-- rendering to html
renderTs :: Int -> Map AccountId Account -> Map TxnId Txn -> Map TxnId Int -> FilterFunc -> SortFunc -> Html
renderTs maxN accMap txnMap balanceMap ff sf =
    let ts = sortBy sf $ filter ff $ Map.elems txnMap
    in
        H.div ! A.id "container" ! A.style "font-family: \"Inconsolata\", monospace;" $ do
            H.div $ toHtml (show (length ts) ++ " transactions " ++ show2decimal (sum $ map ((/100) . fromIntegral . view L.amount) ts))
            H.table $ do
                H.th "Id"
                H.th "Date"
                H.th "Description"
                H.th "Amount"
                H.th "Tags"
                H.th "Balance"
                H.th "Account"
                mapM_ (\t -> renderT accMap t (Map.lookup (t ^. L.id) balanceMap)) (take maxN ts)

ellipsis :: Int -> String -> String
ellipsis n s = if length s > n then take (n - 3) s ++ "..." else s

renderT :: Map AccountId Account -> Txn -> Maybe Int -> Html
renderT accMap t mBal =
    H.tr $ do
        let mAcc = Map.lookup (t ^. L.accountId) accMap
            lowBalStyle = "background-color: #ff5050;color: white;"
            isDebitLowBalance = fromMaybe False $ do
                acc <- mAcc
                bal <- mBal
                return $ acc ^. L._type == Debit && bal < 400000
        H.td (toHtml $ t ^. L.id)
        H.td (toHtml $ str $ show (t ^. L.date))
        if T.length (t ^. L.name) > 100 then
            H.td ! A.title (fromString $ T.unpack (t ^. L.name)) $
                (toHtml $ ellipsis 100 $ T.unpack (t ^. L.name))
        else
            H.td (toHtml (t ^. L.name))
        H.td (toHtml $ show2decimal $ (/100) . fromIntegral $ t ^. L.amount)
        H.td (toHtml $ str $ show . Set.toList $ t ^. L.tags)
        H.td ! A.style (if isDebitLowBalance then lowBalStyle else "") $ (toHtml $ fromMaybe "_" $ fmap (show2decimal . (\x -> (fromIntegral x/100 :: Double))) mBal)
        H.td (toHtml $ fromMaybe "_" $ view L.number <$> Map.lookup (t ^. L.accountId) accMap)
    where
        str :: String -> String
        str = identity
