{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Money
    ()
    where

import           Control.Exception (catch)

import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.Bits ((.|.))
import qualified Data.ByteString as BS
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Either (isLeft, lefts, rights)
import           Data.Function (on)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (foldl', groupBy, nub, sortBy, tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import           Data.SafeCopy (SafeCopy(..), Migrate(..), contain, extension, safeGet, safePut)
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, UTCTime (..))
import           Data.Time.Calendar (addDays, addGregorianMonthsClip, diffDays, toGregorian, fromGregorian)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

import           GHC.Generics (Generic)

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Numeric (showFFloat)

import           Servant.API ((:>), ReqBody, Post, JSON, FormUrlEncoded)
import           Servant.Client (BaseUrl(..), ClientEnv(..), ClientM, Scheme(Https), ServantError, client, runClientM)

import           System.Posix.Files (createSymbolicLink)
import           System.Directory (removeFile)

import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Regex.Posix (compIgnoreCase, defaultCompOpt, defaultExecOpt, makeRegexOpts, match)

import           Web.FormUrlEncoded (ToForm(..))

parseTime :: String -> String -> Maybe Day
parseTime f = fmap utctDay . parseTimeM True defaultTimeLocale f

-- anding, and oring filter functions
fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f1 f2 x = f1 x && f2 x

(^&) = fAnd

fOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fOr f1 f2 x = f1 x || f2 x

(^|) = fOr

-- DB operations
type Tag = CI Text

data Txn_v0 = Txn_v0 { plaidTxn_v0 :: PlaidTransaction, tags_v0 :: Set Tag, link_v0 :: Maybe Text } deriving (Eq, Show)
data Txn = Txn {
    plaidTxn          :: PlaidTransaction
  , tags              :: Set Tag
  , link              :: Maybe Text
  , betterDescription :: Maybe Text
  , txnBalance        :: Double -- Balance for this account after transaction completed
  } deriving (Eq, Show)

-- pseudo lens
txnId :: Txn -> TxnId
txnId = txn_id . plaidTxn

type TxnId = Text
type AccId = Text
type TxnDB_v0 = Map TxnId Txn_v0
type TxnDB = Map TxnId Txn
type AccDB = Map AccId PlaidAccount
data DB_v0 = DB_v0 { txnDB_v0 :: TxnDB_v0, accDB_v0 :: AccDB }
data DB = DB { txnDB :: TxnDB, accDB :: AccDB }

emptyDB :: DB
emptyDB = DB Map.empty Map.empty

mkAccDB :: [PlaidAccount] -> AccDB
mkAccDB = Map.fromList . map (\acc -> (acc_id acc, acc))

-- pseudo lens
txns :: DB -> [Txn]
txns = map snd . Map.toList . txnDB

accs :: DB -> [PlaidAccount]
accs = map snd . Map.toList . accDB

date' :: Txn -> Day
date' = fromMaybe (error "parseTime failed") . parseTime "%Y-%m-%d" . T.unpack . date . plaidTxn

instance (CI.FoldCase s, SafeCopy s) => SafeCopy (CI s) where
    putCopy ci = contain $ safePut (CI.foldedCase ci)
    getCopy = contain $ CI.mk <$> safeGet

instance SafeCopy Value where
    putCopy v = contain $ safePut (Aeson.encode v)
    getCopy = contain $ fromMaybe (object []) . Aeson.decode <$> safeGet

instance SafeCopy PlaidTransactionMeta where
    putCopy PlaidTransactionMeta{..} = contain $ safePut txnMeta_location
    getCopy = contain $ PlaidTransactionMeta <$> safeGet

instance SafeCopy PlaidTransactionType where
    putCopy PlaidTransactionType{..} = contain $ safePut primary
    getCopy = contain $ PlaidTransactionType <$> safeGet

instance SafeCopy PlaidTransactionScore where
    putCopy PlaidTransactionScore{..} = contain $ do 
        safePut txnScore_location
        safePut txnScore_name
    getCopy = contain $ PlaidTransactionScore <$> safeGet <*> safeGet

instance SafeCopy PlaidTransaction where
    putCopy PlaidTransaction{..} = contain $ do
        safePut _account
        safePut _pendingTransaction
        safePut txn_id
        safePut amount
        safePut date                
        safePut txn_name            
        safePut txn_meta            
        safePut pending             
        safePut txn_type            
        safePut category            
        safePut category_id         
        safePut score
    getCopy = contain $ PlaidTransaction <$> 
        safeGet <*> safeGet <*> safeGet <*> safeGet <*>
        safeGet <*> safeGet <*> safeGet <*> safeGet <*>
        safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy Txn_v0 where
     putCopy Txn_v0{..} = contain $ do safePut plaidTxn_v0; safePut tags_v0; safePut link_v0
     getCopy = contain $ Txn_v0 <$> safeGet <*> safeGet <*> safeGet
     
instance SafeCopy Txn where
     putCopy Txn{..} = contain $ do
         safePut plaidTxn
         safePut tags
         safePut link
         safePut betterDescription
         safePut txnBalance
     getCopy = contain $ Txn <$>
         safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy PlaidBalance where
    putCopy PlaidBalance{..} = contain $ do safePut available; safePut current
    getCopy = contain $ PlaidBalance <$> safeGet <*> safeGet

instance SafeCopy PlaidAccountMeta where
    putCopy PlaidAccountMeta{..} = contain $ do
        safePut limit
        safePut accMeta_name
        safePut number
        safePut official_name
    getCopy = contain $ PlaidAccountMeta <$>
        safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy PlaidAccount where
    putCopy PlaidAccount{..} = contain $ do
        safePut acc_id
        safePut _item
        safePut _user
        safePut balance
        safePut institution_type
        safePut acc_meta
        safePut subtype
        safePut acc_type
    getCopy = contain $ PlaidAccount <$>
        safeGet <*> safeGet <*> safeGet <*> safeGet <*>
        safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy DB_v0 where
    putCopy DB_v0{..} = contain $ do safePut txnDB_v0; safePut accDB_v0;
    getCopy = contain $ DB_v0 <$> safeGet <*> safeGet

instance SafeCopy DB where
    version = 2
    kind = extension
    putCopy DB{..} = contain $ do safePut txnDB; safePut accDB;
    getCopy = contain $ DB <$> safeGet <*> safeGet

instance Migrate DB where
     type MigrateFrom DB = DB_v0
     migrate DB_v0{..} = calculateTxnBalances $ DB (Map.map (\Txn_v0{..} -> Txn plaidTxn_v0 tags_v0 link_v0 Nothing 0) txnDB_v0) accDB_v0

-- This function also deletes all transactions in the db that map to a non-existing account
-- i.e. 
--    if Map.lookup (_account . plaidTxn $ t) (accDB db) == Nothing
--    then t is deleted
calculateTxnBalances :: DB -> DB
calculateTxnBalances db =
    let outTxns =
            concatMap
                (\(accId, acc) ->
                    let
                        sortedTxns = sortDesc $ filter ((== accId) . _account . plaidTxn) $ txns db
                    in
                        snd $ foldl'
                            (\(curBal, out) Txn{..} ->
                                ((case acc_type acc of -- depository balance is balance in account, credit balance is total balance on credit card to be paid
                                    "depository" -> curBal + amount plaidTxn
                                    "credit" -> curBal - amount plaidTxn
                                    otherwise -> error "unknown account type"), (Txn plaidTxn tags link betterDescription curBal):out)) -- TODO use `over` here
                            (current $ balance acc, [])
                            sortedTxns)
                (Map.toList (accDB db))
    in DB (Map.fromList $ map (\t -> (txnId t, t)) outTxns) (accDB db)

serializeDB :: FilePath -> DB -> IO ()
serializeDB fn = BS.writeFile fn . runPut . safePut

deserializeDB :: FilePath -> IO (Either String DB)
deserializeDB fn = runGet safeGet <$> BS.readFile fn

type DBOp = DB -> DB

txnDBOp :: (a -> Txn -> Txn) -> [Txn] -> a -> DBOp
txnDBOp f ts a DB{..} =
    DB
        (Set.foldr
            (Map.update (Just . f a))
            txnDB
            (Set.fromList $ map txnId ts))
        accDB

addTags :: [Txn] -> Set Tag -> DBOp
addTags = txnDBOp (\tagsToAdd Txn{..} -> Txn plaidTxn (Set.union tagsToAdd tags) link betterDescription txnBalance)

removeTags :: [Txn] -> Set Tag -> DBOp
removeTags = txnDBOp (\tagsToRemove Txn{..} -> Txn plaidTxn (tags `Set.difference` tagsToRemove) link betterDescription txnBalance)

mergeNewResponses :: [PlaidResponse] -> DBOp
mergeNewResponses resps DB{..} =
    calculateTxnBalances $ DB 
        (foldl'
            (\curDb t -> 
                maybe
                    (Map.insert (txn_id t) (Txn t Set.empty Nothing Nothing 0) curDb)
                    (\existingTxn ->
                        bool
                            (error $ "txn mismatch: " ++ show existingTxn ++ " /= " ++ show t)
                            curDb
                            (plaidTxn existingTxn == t))
                    (Map.lookup (txn_id t) curDb))
            txnDB
            (concatMap transactions resps))
        (mkAccDB $ concatMap accounts resps)

-- psuedoLenses
acc :: AccId -> DB -> PlaidAccount
acc accId = fromMaybe (error "invalid accId") . Map.lookup accId . accDB

accNumber :: AccId -> DB -> Text
accNumber accId = number . acc_meta . acc accId 

accBalance :: AccId -> DB -> Double
accBalance accId = current . balance . acc accId

accName :: AccId -> DB -> Text
accName accId = (\x -> accMeta_name x <> " " <> number x) . acc_meta . acc accId

-- fitering and sorting
type FilterFunc = Txn -> Bool
type SortFunc = [Txn] -> [Txn]

fAccount :: DB -> Text -> FilterFunc
fAccount db givenAcc = (givenAcc ==) . flip accNumber db . _account . plaidTxn

fAmount :: (Double -> Bool) -> FilterFunc
fAmount f = f . amount . plaidTxn

fDate :: (Day -> Bool) -> FilterFunc
fDate f = f . date'

fDate' :: ((Integer, Int, Int) -> Bool) -> FilterFunc
fDate' f = f . toGregorian . date'

fMonth :: Integer -> Int -> FilterFunc
fMonth year month = fDate' (\(y, m, _) -> y == year && m == month)

fDesc :: String -> FilterFunc
fDesc reg = match (makeRegexOpts (defaultCompOpt .|. compIgnoreCase) defaultExecOpt reg) . T.unpack . txn_name . plaidTxn

fTag :: Tag -> FilterFunc
fTag tag = Set.member tag . tags

sortDesc :: [Txn] -> [Txn]
sortDesc = sortBy (\a b -> (flip compare `on` date') a b <> (flip compare `on` txnId) a b)

sortAsc :: [Txn] -> [Txn]
sortAsc = sortBy (\a b -> (compare `on` date') a b <> (compare `on` txnId) a b)

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


-- hits plaid and saves response to file
makeApiCall :: IO [PlaidResponse]
makeApiCall = do
    timeStr <- formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S_%Z" <$> getCurrentTime
    apiResps <- plaidGet plaidCreds
    let fname = "apiResps-" ++ timeStr ++ ".hson"
    let linkname = "money-responses/latest.hson"
    bool (writeFile ("money-responses/" ++ fname) (show $ rights apiResps) >>
            removeFile linkname `catch` (\(e :: IOError) -> return ()) >>
            createSymbolicLink fname linkname >>
            return (rights apiResps))
        (writeFile "makeApiCall.log" (show (lefts apiResps)) >> error "atleast one access token failed see makeApiCall.log")
        (any isLeft apiResps)

backupAndSerializeDB :: IORef DB -> IO ()
backupAndSerializeDB dbIORef = do
    timeStr <- formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S_%Z" <$> getCurrentTime
    let fname = "transactions-" ++ timeStr ++ ".db"
    let linkname = "money-dbs/latest.db"
    serializeDB ("money-dbs/" ++ fname) =<< readIORef dbIORef
    removeFile linkname `catch` (\(e :: IOError) -> return ())
    createSymbolicLink fname linkname
    
updateDBFromPlaid :: IORef DB -> IO ()
updateDBFromPlaid dbIORef = do
    db <- readIORef dbIORef
    plaidResps <- makeApiCall
    writeIORef dbIORef $ mergeNewResponses plaidResps db
    backupAndSerializeDB dbIORef

{-
dbIORef <- newIORef =<< (either (error . show) id <$> deserializeDB "money-dbs/latest.db")

updateDBFromPlaid dbIORef -- calls backupAndSerializeDB internally

backupAndSerializeDB dbIORef

-- addTags
justForScope = do
    db <- readIORef dbIORef
    writeIORef dbIORef
        (addTags 
            (filter (fDesc "doordash") $ txns db)
            ["food"]
            db)
justForScope

-- removeTags
justForScope = do
    db <- readIORef dbIORef
    writeIORef dbIORef
        (removeTags 
            (filter (fDesc "lyft") $ txns db)
            ["lulshare"]
            db)
justForScope

justForScope = do
    db <- readIORef dbIORef
    let ff = fAccount db "6150"
    let ts = sortDesc $ filter ff (txns db)
    return $ renderTs 1000 db ts
justForScope

-- run recurring transaction detection
justForScope = do
    db <- readIORef dbIORef
    let ff = fDate (> (2016, 6, 1))
    let ts = filter ff (txns db)
    mapM_ (print . (\(t, s) -> (txn_name $ plaidTxn t, amount $ plaidTxn t, s))) $
        sortBy (flip compare `on` snd) $
            map (\t -> (t, recurringScore ts t)) ts
justForScope

-- transaction prediction WIP
{-
justForScope = do
    db <- readIORef dbIORef
    let fml = fromMaybe undefined
    let d0 = fml $ parseTime "%Y-%m-%d" "2017-04-07"
    return $ take 10 $ map (formatTime defaultTimeLocale "%Y-%m-%d" . (`addDays` d0)) $ iterate (+ 7) 0
-}

-- TODO cleanup and documentation for this function
type DayOfMonth = Int
type Year = Integer
type Month = Int
calculateCCBill :: DayOfMonth -> DayOfMonth -> DayOfMonth -> Text -> String -> (Year, Month) -> IO [Txn]
calculateCCBill cycleStart cycleEnd billDueD accountNum paymentDesc (billDueY, billDueM) = do
    db <- readIORef dbIORef
    let billStart = fromGregorian billDueY billDueM cycleStart
    let billEnd = fromGregorian billDueY billDueM cycleEnd
    let billDue = fromGregorian billDueY billDueM billDueD
    let dateFilter = fDate ((>= addGregorianMonthsClip (-2) billStart)
                            ^& (<= addGregorianMonthsClip (-1) billEnd))
    let creditsSinceBillEnd =
            fDate ((> addGregorianMonthsClip (-1) billEnd)
                ^& (< billDue)) ^& fAmount (< 0)
    let previousCredits =
            fDate ((>= addGregorianMonthsClip (-2) billStart)
                  ^& (< addGregorianMonthsClip (-1) billDue)) ^& fAmount (< 0)
    let ff = fAccount db accountNum ^& (not . fDesc paymentDesc) ^& (dateFilter ^| creditsSinceBillEnd) ^& (not . previousCredits)
    return $ filter ff (txns db)

-- 0881 billCalc: 20th -> 19th, bill due 14th, payment desc: "PAYMENT - THANK YOU"
-- 4231 billCalc: 07th -> 06th, bill due 03rd, payment desc: "AUTOMATIC PAYMENT - THANK"
-- 8421 billCalc: 13th -> 12th, bill due 09th, payment desc: "AUTOMATIC PAYMENT - THANK"
bofaCCBill = calculateCCBill 20 19 14 "0881" "PAYMENT - THANK YOU"
amazonCCBill = calculateCCBill 7 6 3 "4231" "AUTOMATIC PAYMENT - THANK"
unitedCCBill = calculateCCBill 13 12 9 "8421" "AUTOMATIC PAYMENT - THANK"
sapphireCCBill = calculateCCBill 25 24 21 "4569" "AUTOMATIC PAYMENT - THANK"

justForScope = do
    db <- readIORef dbIORef
    return $ renderTs 1000 db . sortDesc <$> amazonCCBill (2017, 5)
justForScope

justForScope = do
    resps :: [PlaidResponse] <- read <$> readFile "money-responses/latest.hson"
    db <- readIORef dbIORef
    mapM_ print $ accDB db
    print $ length $ filter (("ejgkL9Y6Ovs87nLezd6yHrKAzk4V6Zh4X05kd" ==) . _account) $ concatMap transactions resps
justForScope
-}
