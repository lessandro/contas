module Contas.LastDB (getThing, getLastThing, appendThing) where

import System.IO

db :: IOMode -> (Handle -> IO r) -> IO r
db = withFile "/tmp/lastdb"

formatThing :: (String, Integer) -> String
formatThing (line, pos) = show pos ++ " " ++ line

getCharAt :: Handle -> Integer -> IO Char
getCharAt _ (-1) = return '\n'
getCharAt h pos = do
    hSeek h AbsoluteSeek pos
    hLookAhead h

-- Read a line backwards
rGetLine :: Handle -> Integer -> IO (String, Integer)
rGetLine h pos = do
    c <- getCharAt h $ pos - 1
    case c of
        '\n' -> return ("", pos-1)
        _ -> do
            (ln, pos') <- rGetLine h $ pos - 1
            return (ln ++ [c], pos')

fileSize :: Handle -> IO Integer
fileSize h = do
    hSeek h SeekFromEnd 0
    hTell h

getThing :: Integer -> IO String
getThing (-1) = return "-1"
getThing pos = db ReadMode $ \h -> do
    thing <- rGetLine h pos
    return $ formatThing thing

getLastThing :: IO String
getLastThing = db ReadMode $ \h -> do
    pos <- fileSize h
    thing <- rGetLine h $ pos - 1
    return $ formatThing thing

appendThing :: String -> IO ()
appendThing thing = db AppendMode $ \h ->
    hPutStrLn h thing
