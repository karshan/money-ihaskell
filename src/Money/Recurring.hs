module Money.Recurring
    where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (tails)
import Data.Function (on)
import Data.Time.Calendar (diffDays, toGregorian)

import Money.Plaid
import Money.DB.Types
import Money.Lens

-- Recurring transaction detection
fuzzyMatch :: Text -> Text -> Double
fuzzyMatch a b = fromIntegral countEqual/normalizer
    where
        maxLen = fromIntegral $ max (length (T.words a)) (length (T.words b))
        pluralize = tails . T.words
        permutations = mapM pluralize [a, b]
        countEqual = (sum . map (listApp match)) permutations
        match as bs = count True $ zipWith (==) as bs -- figured this out by trial and error. this is n*(n+1)/2 which is sum (map length (tails (words a)))
        normalizer = maxLen * (maxLen + 1) * 0.5
        listApp f xs = f (head xs) (last xs)
        count e xs = length $ filter (==e) xs

numMatch :: Double -> Double -> Double
numMatch a b = 1 - (abs (a - b)/max (abs a) (abs b))

nameMatch :: Txn -> Txn -> Double
nameMatch t1 t2 = (fuzzyMatch `on` (txn_name . plaidTxn)) t1 t2

amountMatch :: Txn -> Txn -> Double
amountMatch t1 t2 = (numMatch `on` (amount . plaidTxn)) t1 t2

dateMatch :: Txn -> Txn -> Double
dateMatch t1 t2 =
    let dd = abs (diffDays (date' t1) (date' t2))
        dayOfMonth = (\(_, _, x) -> x) . toGregorian . date'
        numMatchI a b = numMatch (fromIntegral a) (fromIntegral b)
    in maximum $ [(numMatchI `on` dayOfMonth) t1 t2, numMatchI (dayOfMonth t1 + 30) (dayOfMonth t2), numMatchI (dayOfMonth t1) (dayOfMonth t2 + 30)] ++
        (map (\x -> numMatchI (dd `mod` x) x) [14, 7])

recurringScore :: [Txn] -> Txn -> Double
recurringScore ts t1 =
    avg (map (\t -> dateMatch t t1) $ nameAmtMatches) - if length nameAmtMatches <= 2 then 0.2 else 0
    where
        avg xs = if length xs == 0 then 0 else sum xs/fromIntegral (length xs)
        nmCut = 0.7
        amtCut = 0.9
        dtCut = 0.8
        nameAmtMatches = filter (\t -> nameMatch t t1 > nmCut && amountMatch t t1 > amtCut && t /= t1) ts
        --nadMatches = filter (\t -> nameMatch t t1 > nmCut && amountMatch t t1 > amtCut && dateMatch t t1 > dtCut && t /= t1) ts


