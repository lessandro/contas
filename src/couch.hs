{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Contas.Couch where

import Database.CouchDB
import Data.Data (Data, Typeable)

import Text.JSON
import Text.JSON.Generic (toJSON, fromJSON, decodeJSON)

data Item = Item { blob, ref :: String }
    deriving (Eq, Show, Data, Typeable)

contas = db "contas"

getItem ::  String -> IO Item
getItem ref = do
    Just raw <- runCouchDB' $ getDocRaw contas $ doc ref
    return $ (decodeJSON raw :: Item)

getCurrentRef :: IO String
getCurrentRef = do
    item <- getItem "current"
    return $ ref item

updateCurrent :: String -> IO ()
updateCurrent newRef = do
    Just (doc1, rev1, json1 :: JSValue) <- runCouchDB' $ getDoc contas $ doc "current"
    let (Ok (item1 :: Item)) = fromJSON json1
    let json2 = toJSON item1 { ref = newRef }
    Just (doc2, rev2) <- runCouchDB' $ updateDoc contas (doc1, rev1) json2
    return ()

addNew :: String -> IO ()
addNew newBlob = do
    current <- getCurrentRef
    let item = Item newBlob current
    (doc1, rev1) <- runCouchDB' $ newDoc contas $ toJSON item
    updateCurrent $ show doc1
    return ()

getBlob :: String -> IO String
getBlob ref = do
    item <- getItem ref
    return $ blob item

getCurrentBlob :: IO String
getCurrentBlob = do
    current <- getCurrentRef
    getBlob current
