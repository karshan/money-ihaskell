module Money.FilterSort
    where

import           Data.Function (on)
import           Data.Monoid ((<>))
import           Data.List (foldl', groupBy, nub, sortBy, tails)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Data.Bits ((.|.))
import           Text.Regex.Posix (compIgnoreCase, defaultCompOpt, defaultExecOpt, makeRegexOpts, match)
import           Data.Time (Day)
import           Data.Time.Calendar (addDays, addGregorianMonthsClip, diffDays, toGregorian, fromGregorian)

import           Money.Lens
import           Money.DB.Types
import           Money.Plaid

-- anding, and oring filter functions
fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f1 f2 x = f1 x && f2 x

(^&) = fAnd

fOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fOr f1 f2 x = f1 x || f2 x

(^|) = fOr

-- fitering and sorting
type FilterFunc = Txn -> Bool
type SortFunc = [Txn] -> [Txn]

fAccount :: DB -> Text -> FilterFunc
fAccount db givenAcc = (Just givenAcc ==) . flip accNumber' db . accountId

fAmount :: (Double -> Bool) -> FilterFunc
fAmount f = f . (/100) . fromIntegral . amount

fDate :: (Day -> Bool) -> FilterFunc
fDate f = f . date

fDate' :: ((Integer, Int, Int) -> Bool) -> FilterFunc
fDate' f = f . toGregorian . date

fMonth :: Integer -> Int -> FilterFunc
fMonth year month = fDate' (\(y, m, _) -> y == year && m == month)

fDesc :: String -> FilterFunc
fDesc reg = match (makeRegexOpts (defaultCompOpt .|. compIgnoreCase) defaultExecOpt reg) . T.unpack . desc

fTag :: Tag -> FilterFunc
fTag tag = Set.member tag . tags

sortDesc :: [Txn] -> [Txn]
sortDesc = sortBy (\a b -> (flip compare `on` date) a b <> (flip compare `on` txnId) a b)

sortDesc_v1 :: [Txn_v1] -> [Txn_v1]
sortDesc_v1 = sortBy (\a b -> (flip compare `on` date_v1) a b <> (flip compare `on` (txn_id . plaidTxn_v1)) a b)

sortAsc :: [Txn] -> [Txn]
sortAsc = sortBy (\a b -> (compare `on` date) a b <> (compare `on` txnId) a b)


