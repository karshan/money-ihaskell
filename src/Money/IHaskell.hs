{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Money.IHaskell where

import           Numeric (showFFloat)
import qualified Data.Text as T
import           Data.String (fromString)
import qualified Data.Set as Set

import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Money.Lens
import Money.DB.Types
import Money.Plaid

-- rendering to html
renderTs :: Int -> DB -> [Txn] -> Html
renderTs maxN db ts =
    H.div ! A.id "container" ! A.style "font-family: \"Inconsolata\", monospace;" $ do
        H.div $ toHtml (show (length ts) ++ " transactions " ++ showFFloat (Just 2) (sum $ map (amount . plaidTxn) ts) "")
        H.table $ do
            H.th "Id"
            H.th "Date"
            H.th "Description"
            H.th "Amount"
            H.th "Tags"
            H.th "Balance"
            H.th "Account"
            mapM_ (renderT db) (take maxN ts)

ellipsis :: Int -> String -> String
ellipsis n s = if length s > n then take (n - 3) s ++ "..." else s

renderT :: DB -> Txn -> Html
renderT db t =
    go (plaidTxn t) (tags t)
    where
        go PlaidTransaction{..} tags = do
            H.tr $ do
                H.td (toHtml $ T.drop 31 txn_id)
                H.td (toHtml date)
                if T.length txn_name > 100 then
                    H.td ! A.title (fromString $ T.unpack txn_name) $
                        (toHtml $ ellipsis 100 $ T.unpack txn_name)
                else
                    H.td (toHtml txn_name)
                H.td (toHtml amount)
                H.td (toHtml $ show . Set.toList $ tags)
                H.td (toHtml $ showFFloat (Just 2) (txnBalance t) "")
                H.td (toHtml $ accNumber _account db)


