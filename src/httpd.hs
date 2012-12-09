module Contas.Httpd where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Network
import System.IO

port = 1234
server = "lzmhttpd/0.1"

errorHandler :: IOError -> IO String
errorHandler = const $ return ""

readLine :: Handle -> IO String
readLine h = catch (hGetLine h) errorHandler

processConnection :: Handle -> IO ()
processConnection h = do
    line <- readLine h
    putStr $ line ++ "\n"

    case line of
        "hi" -> do
            hClose h
            return ()
            
        "" -> do
            hClose h
            return ()

        _ -> do
            processConnection h

mainLoop :: Socket -> IO ()
mainLoop sock = do
    (handle, _, _) <- accept sock
    putStr "accepted\n"
    forkOS $ processConnection handle
    mainLoop sock

main :: IO ()
main = do
    putStr $ "listening on port " ++ (show port) ++ "\n"
    sock <- listenOn $ PortNumber port
    mainLoop sock
