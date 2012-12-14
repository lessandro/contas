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

decodeItem :: String -> Item
decodeItem = decodeJSON

getRaw :: String -> IO (Maybe String)
getRaw ref = do
    raw <- runCouchDB' $ getDocRaw contas $ doc ref
    return raw

getItem ::  String -> IO (Maybe Item)
getItem ref = do
    raw <- getRaw ref
    return $ liftM decodeItem $ raw

getCurrentRef :: IO (Maybe String)
getCurrentRef = do
    item <- getItem "current"
    return $ liftM ref $ item

updateCurrent :: String -> IO ()
updateCurrent newRef = do
    theDoc <- runCouchDB' $ getDoc contas $ doc "current"
    let Just (doc1, rev1, json1 :: JSValue) = theDoc
    let (Ok (item1 :: Item)) = fromJSON json1
    let json2 = toJSON item1 { ref = newRef }
    Just (doc2, rev2) <- runCouchDB' $ updateDoc contas (doc1, rev1) json2
    return ()

addNewBlob :: String -> IO ()
addNewBlob newBlob = do
    Just current <- getCurrentRef
    let item = Item newBlob current
    (doc1, rev1) <- runCouchDB' $ newDoc contas $ toJSON item
    updateCurrent $ show doc1
    return ()

getBlob :: String -> IO (Maybe String)
getBlob ref = do
    item <- getItem ref
    return $ liftM blob $ item

getCurrentBlob :: IO (Maybe String)
getCurrentBlob = do
    mref <- getCurrentRef
    case mref of
        Just ref -> getBlob ref
        Nothing -> return Nothing
