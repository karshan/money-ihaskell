module Money.Recurring
    where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (tails)
import Data.Function (on)
import Data.Time.Calendar (diffDays, toGregorian)

import MoneySyncService.Types
import qualified Lenses as L
import Control.Lens

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

gdiv :: Real a => a -> a -> Double
gdiv a b = let f = fromRational . toRational in f a/f b

numMatch :: Int -> Int -> Double
numMatch a b = 1 - (abs (a - b) `gdiv` max (abs a) (abs b))

nameMatch :: Txn -> Txn -> Double
nameMatch t1 t2 = (fuzzyMatch `on` (view L.name)) t1 t2

amountMatch :: Txn -> Txn -> Double
amountMatch t1 t2 = (numMatch `on` (view L.amount)) t1 t2

dateMatch :: Txn -> Txn -> Double
dateMatch t1 t2 =
    let dd = fromIntegral $ abs (diffDays (t1 ^. L.date) (t2 ^. L.date))
        dayOfMonth = (\(_, _, x) -> x) . toGregorian . view L.date
    in maximum $ [(numMatch `on` dayOfMonth) t1 t2, numMatch (dayOfMonth t1 + 30) (dayOfMonth t2), numMatch (dayOfMonth t1) (dayOfMonth t2 + 30)] ++
        (map (\x -> numMatch (dd `mod` x) x) [14, 7])

recurringScore :: [Txn] -> Txn -> Double
recurringScore ts t1 =
    avg (map (\t -> dateMatch t t1) $ nameAmtMatches) - if length nameAmtMatches <= 2 then 0.2 else 0
    where
        avg xs = if length xs == 0 then 0 else sum xs/fromIntegral (length xs)
        nmCut = 0.7
        amtCut = 0.9
        nameAmtMatches = filter (\t -> nameMatch t t1 > nmCut && amountMatch t t1 > amtCut && t /= t1) ts
