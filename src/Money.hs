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
    where

import           Control.Exception (catch)

import           Data.Bool (bool)
import           Data.Either (isLeft, lefts, rights)
import           Data.IORef (IORef, readIORef, writeIORef)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           System.Posix.Files (createSymbolicLink)
import           System.Directory (removeFile)

import           Money.Plaid
import           Money.DB
import           Money.DB.Types

plaidCreds :: Creds
plaidCreds = undefined

-- hits plaid and saves response to file
makeApiCall :: IO [PlaidResponse]
makeApiCall = do
    timeStr <- formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S_%Z" <$> getCurrentTime
    apiResps <- plaidGet plaidCreds
    let fname = "apiResps-" ++ timeStr ++ ".hson"
    let linkname = "money-responses/latest.hson"
    bool (writeFile ("money-responses/" ++ fname) (show $ rights apiResps) >>
            removeFile linkname `catch` (\(_ :: IOError) -> return ()) >>
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
    removeFile linkname `catch` (\(_ :: IOError) -> return ())
    createSymbolicLink fname linkname
    
updateDBFromPlaid :: IORef DB -> IO ()
updateDBFromPlaid dbIORef = do
    db <- readIORef dbIORef
    plaidResps <- makeApiCall
    let eNewDb = mergeNewResponses plaidResps db
    either (\s -> putStrLn $ "mergeNewResponse failed: " ++ s) (writeIORef dbIORef) eNewDb
    backupAndSerializeDB dbIORef
