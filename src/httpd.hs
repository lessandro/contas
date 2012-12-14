module Httpd where

import Prelude hiding (catch)
import Data.Char
import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Network
import System.IO

import Couch

port = 20202
server = "lzmhttpd/0.1"

errorHandler :: IOError -> IO String
errorHandler = const $ return ""

dataOrEpsilon :: (Handle -> IO String) -> Handle -> IO String
dataOrEpsilon f h = catch (f h) errorHandler

readLine :: Handle -> IO String
readLine = dataOrEpsilon hGetLine

readUntilCR :: Handle -> IO ()
readUntilCR h = do
    line <- readLine h
    case line of
        "\r" -> return ()
        "" -> return ()
        _ -> readUntilCR h

readMethod :: Handle -> IO (String, String)
readMethod h = do
    line <- readLine h
    let splitted = words line
    case splitted of
        [] -> return ("", "")
        _ -> return (splitted!!0, splitted!!1)

parseKV :: String -> (String, String)
parseKV line
    | splitted == [] = ("", "")
    | otherwise = (key, value)
    where
        splitted = words line
        key = map toLower $ head splitted
        value = last splitted

readLength :: Handle -> IO Int
readLength h = do
    line <- readLine h
    let (key, value) = parseKV line
    case key of
        "" -> return 0
        "content-length:" -> do
            readUntilCR h
            return $ (read value :: Int)
        _ -> readLength h

readChar :: Handle -> IO String
readChar h = do
    ch <- hGetChar h
    return [ch]

readContent :: Handle -> Int -> IO String
readContent h len
    | len == 0 = return ""
    | otherwise = do
        str <- dataOrEpsilon readChar h
        rest <- readContent h (len-1)
        return $ str ++ rest

writeResponse :: Handle -> String -> IO ()
writeResponse h content = do
    hPutStr h "HTTP/1.1 200 OK\r\n"
    hPutStr h "Content-Type: application/json\r\n"
    hPutStr h "Access-Control-Allow-Origin: *\r\n"
    hPutStr h "Access-Control-Allow-Methods: GET, POST\r\n"
    hPutStr h $ "Server: " ++ server ++ "\r\n"
    hPutStr h $ "Content-Length: " ++ (show $ length content) ++ "\r\n"
    hPutStr h "Connection: close\r\n"
    hPutStr h "\r\n"    
    hPutStr h content

doGet path = path
doPost path content = path ++ " " ++ content

processRequest :: String -> String -> String -> String
processRequest method path content
    | method == "GET" = doGet path
    | method == "POST" = doPost path content
    | otherwise = ""

processConnection :: Handle -> IO ()
processConnection h = do
    (method, path) <- readMethod h
    len <- readLength h
    content <- readContent h len

    putStrLn $ "method: " ++ show method
    putStrLn $ "path: " ++ show path
    putStrLn $ "len: " ++ show len
    putStrLn $ "content: " ++ show content
    putStrLn ""

    writeResponse h $ processRequest method path content
    hClose h

mainLoop :: Socket -> IO ()
mainLoop sock = do
    (handle, _, _) <- accept sock
    forkOS $ processConnection handle
    mainLoop sock

main :: IO ()
main = do
    putStr $ "listening on port " ++ (show port) ++ "\n"
    sock <- listenOn $ PortNumber port
    mainLoop sock
