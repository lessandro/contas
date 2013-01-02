module Contas.Httpd (serve) where

import Prelude hiding (catch)
import Data.Char
import Control.Concurrent
import Control.Exception hiding (handle)
import Network
import System.IO

import Contas.LastDB

port :: PortNumber
port = 20202

server :: String
server = "lzmhttpd/0.1"

-- IOError handler
errorHandler :: IOError -> IO String
errorHandler = const $ return ""

-- Try to read from a handle, return "" if that fails
dataOrEpsilon :: (Handle -> IO String) -> Handle -> IO String
dataOrEpsilon f h = catch (f h) errorHandler

-- Read a char from a handle
readChar :: Handle -> IO String
readChar h = do
    ch <- hGetChar h
    return [ch]

-- Read a line from a handle
readLine :: Handle -> IO String
readLine = dataOrEpsilon hGetLine

-- Keep reading lines until "\r" is found
readUntilCR :: Handle -> IO ()
readUntilCR h = do
    line <- readLine h
    case line of
        "\r" -> return ()
        "" -> return ()
        _ -> readUntilCR h

-- Read and decode things like "GET / HTTP/1.0\r"
readMethod :: Handle -> IO (String, String)
readMethod h = do
    line <- readLine h
    let splitted = words line
    case splitted of
        [] -> return ("", "")
        (_:[]) -> return ("",  "")
        _ -> return (head splitted, splitted!!1)

-- Decode things like "Content-Length: 1234"
parseKV :: String -> (String, String)
parseKV line
    | null splitted = ("", "")
    | otherwise = (key, value)
    where
        splitted = words line
        key = map toLower $ head splitted
        value = last splitted

-- Read lines until a "Content-Length" header is found, and then keep reading
-- lines until the end of the HTTP header.
-- Return 0 if a content-length header is not present.
readLength :: Handle -> IO Int
readLength h = do
    line <- readLine h
    let (key, value) = parseKV line
    case key of
        "" -> return 0
        "content-length:" -> do
            readUntilCR h
            return (read value :: Int)
        _ -> readLength h

-- Read the content part of the HTTP post
readContent :: Handle -> Int -> IO String
readContent h len
    | len == 0 = return ""
    | len > 32768 = return ""
    | otherwise = do
        str <- dataOrEpsilon readChar h
        rest <- readContent h (len-1)
        return $ str ++ rest

-- Write the HTTP response
writeResponse :: Handle -> String -> IO ()
writeResponse h content = do
    hPutStr h "HTTP/1.1 200 OK\r\n"
    hPutStr h "Content-Type: text/plain\r\n"
    hPutStr h "Access-Control-Allow-Origin: *\r\n"
    hPutStr h "Access-Control-Allow-Methods: GET, POST\r\n"
    hPutStr h "Access-Control-Allow-Headers: Content-Length, Content-Type\r\n"
    hPutStr h $ "Server: " ++ server ++ "\r\n"
    hPutStr h $ "Content-Length: " ++ show (length content) ++ "\r\n"
    hPutStr h "Connection: close\r\n"
    hPutStr h "\r\n"    
    hPutStr h content

-- Get a document from the CouchDB
-- If no path is specified ("/"), return the current document
doGet :: String -> IO String
doGet "/" = getLastThing
doGet ('/':path) = getThing (read path :: Integer)
doGet _ = return ""

-- Save a new document
doPost :: String -> IO String
doPost content = do
    appendThing content
    return "true"

-- Process the HTTP request
processRequest :: String -> String -> String -> IO String
processRequest "GET" path _ = doGet path
processRequest "POST" _ content = doPost content
processRequest _ _ _ = return ""

-- Process a HTTP connection
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

    response <- catch (processRequest method path content) errorHandler

    putStrLn $ "response: " ++ show response
    putStrLn ""

    writeResponse h response
    hClose h

-- Accept/fork loop
acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (handle, _, _) <- accept sock
    _ <- ($) forkOS $ processConnection handle
    acceptLoop sock

serve :: IO ()
serve = do
    putStr $ "listening on port " ++ show port ++ "\n"
    sock <- listenOn $ PortNumber port
    acceptLoop sock
