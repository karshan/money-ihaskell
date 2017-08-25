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

import           Data.Bool (bool)
import           Data.Either (isLeft, lefts, rights)
import           Data.Function (on)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (foldl', groupBy, nub, sortBy, tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, UTCTime (..))
import           Data.Time.Calendar (addDays, addGregorianMonthsClip, diffDays, toGregorian, fromGregorian)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

import           System.Posix.Files (createSymbolicLink)
import           System.Directory (removeFile)

import           Money.Plaid
import           Money.DB
import           Money.DB.Types
import           Money.Lens
import           Money.FilterSort
import           Money.Recurring
import           Money.IHaskell
import           Money.Prediction

plaidCreds = undefined

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
    let eNewDb = mergeNewResponses plaidResps db
    either (\s -> putStrLn $ "mergeNewResponse failed: " ++ s) (writeIORef dbIORef) eNewDb
    backupAndSerializeDB dbIORef
