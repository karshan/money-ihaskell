{-# LANGUAGE NoImplicitPrelude #-}
module Money.FilterSort
    where

import           Data.Function (on)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Data.Bits ((.|.))
import           Text.Regex.Posix (compIgnoreCase, defaultCompOpt, defaultExecOpt, makeRegexOpts, match)
import           Data.Time (Day)
import           Data.Time.Calendar (toGregorian)

import           MoneySyncService.Types
import qualified Lenses as L
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Protolude

-- anding, and oring filter functions
fAnd, (^&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f1 f2 x = f1 x && f2 x

(^&) = fAnd

fOr, (^|) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fOr f1 f2 x = f1 x || f2 x

(^|) = fOr

-- fitering and sorting
type FilterFunc = Txn -> Bool
type SortFunc = (Txn -> Txn -> Ordering)

fAccount :: Map AccountId Account -> Text -> FilterFunc
fAccount db givenAcc = (Just givenAcc ==) . fmap (view L.number) . flip Map.lookup db . view L.accountId

fAmount :: (Double -> Bool) -> FilterFunc
fAmount f = f . (/100) . fromIntegral . view L.amount

fDate :: (Day -> Bool) -> FilterFunc
fDate f = f . view L.date

fDate' :: ((Integer, Int, Int) -> Bool) -> FilterFunc
fDate' f = f . toGregorian . view L.date

fMonth :: Integer -> Int -> FilterFunc
fMonth year month = fDate' (\(y, m, _) -> y == year && m == month)

fDesc :: [Char] -> FilterFunc
fDesc reg = match (makeRegexOpts (defaultCompOpt .|. compIgnoreCase) defaultExecOpt reg) . T.unpack . view L.name

fTag :: Tag -> FilterFunc
fTag tag = Set.member tag . view L.tags

sortDesc :: SortFunc
sortDesc = (\a b -> (flip compare `on` (view L.date)) a b <> (flip compare `on` (view L.id)) a b)

sortAsc :: SortFunc
sortAsc = (\a b -> (compare `on` (view L.date)) a b <> (compare `on` (view L.id)) a b)
