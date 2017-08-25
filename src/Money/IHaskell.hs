{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Money.IHaskell where

import           Numeric (showFFloat)
import           Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.String (fromString)
import qualified Data.Set as Set

import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Money.Lens
import Money.DB.Types
import Money.Balance
import Money.FilterSort (FilterFunc, SortFunc)

show2decimal :: Double -> String
show2decimal x = showFFloat (Just 2) x ""

-- rendering to html
renderTs :: Int -> DB -> FilterFunc -> SortFunc -> Html
renderTs maxN db ff sf =
    let balanceMap = calculateTxnBalances db
        ts = sortBy sf $ filter ff (txns db)
    in
        H.div ! A.id "container" ! A.style "font-family: \"Inconsolata\", monospace;" $ do
            H.div $ toHtml (show (length ts) ++ " transactions " ++ show2decimal (sum $ map amount' ts))
            H.table $ do
                H.th "Id"
                H.th "Date"
                H.th "Description"
                H.th "Amount"
                H.th "Tags"
                H.th "Balance"
                H.th "Account"
                mapM_ (\t -> renderT db t (Map.lookup (txnId t) balanceMap)) (take maxN ts)

ellipsis :: Int -> String -> String
ellipsis n s = if length s > n then take (n - 3) s ++ "..." else s

renderT :: DB -> Txn -> Maybe Int -> Html
renderT db t@Txn{..} mBal =
    H.tr $ do
        H.td (toHtml $ T.drop 31 txnId)
        H.td (toHtml $ show date)
        if T.length desc > 100 then
            H.td ! A.title (fromString $ T.unpack desc) $
                (toHtml $ ellipsis 100 $ T.unpack desc)
        else
            H.td (toHtml desc)
        H.td (toHtml $ amount' t)
        H.td (toHtml $ show . Set.toList $ tags)
        H.td (toHtml $ maybe "_" id $ fmap (show2decimal . (\x -> (fromIntegral x/100 :: Double))) mBal)
        H.td (toHtml $ maybe "_" id $ accNumber' accountId db)


