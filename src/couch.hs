{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Couch where

import Control.Monad
import Data.Data (Data, Typeable)
import Database.CouchDB
import Text.JSON
import Text.JSON.Generic (toJSON, fromJSON, decodeJSON)

data Item = Item { blob, ref :: String }
    deriving (Eq, Show, Data, Typeable)

contas = db "contas"

-- Transform a json string into an Ttem
decodeItem :: String -> Item
decodeItem = decodeJSON

-- Get the whole document data
getRaw :: String -> IO (Maybe String)
getRaw ref = do
    raw <- runCouchDB' $ getDocRaw contas $ doc ref
    return raw

-- Get the whole document data as an Item
getItem ::  String -> IO (Maybe Item)
getItem ref = do
    raw <- getRaw ref
    return $ liftM decodeItem $ raw

-- Get the ref field of the current doc
getCurrentRef :: IO (Maybe String)
getCurrentRef = do
    item <- getItem "current"
    return $ liftM ref $ item

-- Change the "current" doc ref to newRef
updateCurrent :: String -> IO ()
updateCurrent newRef = do
    theDoc <- runCouchDB' $ getDoc contas $ doc "current"
    let Just (doc1, rev1, json1 :: JSValue) = theDoc
    let (Ok (item1 :: Item)) = fromJSON json1
    let json2 = toJSON item1 { ref = newRef }
    Just (doc2, rev2) <- runCouchDB' $ updateDoc contas (doc1, rev1) json2
    return ()

-- Add a new doc with given blob.
-- The new doc points to the previous current doc and current now points
-- to the new doc
addNewBlob :: String -> IO ()
addNewBlob newBlob = do
    Just current <- getCurrentRef
    let item = Item newBlob current
    (doc1, rev1) <- runCouchDB' $ newDoc contas $ toJSON item
    updateCurrent $ show doc1
    return ()
