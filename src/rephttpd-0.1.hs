module Main where

import Control.Monad.State
import Data.List
import Network
import Network.BSD
import Network.Socket
import qualified Data.Map as M
import Data.Char
import System.IO
import qualified System.IO.Error as IOE
import Foreign.Marshal.Alloc
import Control.Concurrent
import System.Time
import System.Process
import System.Posix.IO
import System.Directory

path = "/Users/lzm/Sites"
port = 1234
httpdVer = "rephttpd/0.1"

data ParserState = ParserState {
    method :: String,
    location :: String,
    headers :: M.Map String String,
    httpVer :: String,
    err :: Int,
    clientHost :: HostName
  } deriving (Show)

type Parser = StateT ParserState IO

emptyState = ParserState "" "" M.empty "" 0 ""

parseHttp :: String -> Parser ()
parseHttp x =
    case (last $ head $ words x) of
             ':' -> parseHeader x
             _ -> parseMethod x

parseHeader :: String -> Parser ()
parseHeader x =
    get >>= \s -> put $ s { headers = M.insert k v (headers s) }
        where (y:ys) = words x
              k = reverse $ tail $ reverse y
              v = filter (/= '\r') $ concat ys 

parseMethod :: String -> Parser ()
parseMethod x =
    parseMethod' (words x)
        where
        parseMethod' (m:l:v:[]) = get >>= \s -> put $
                                  s { method = m, location = l, httpVer = v }
        parseMethod' _ = get >>= \s -> put $ s { err = 400 } -- Bad request

sendCommonHeaders :: Handle -> IO ()
sendCommonHeaders h =
    do ctime <- getClockTime
       caltime <- toCalendarTime ctime
       hPutStr h $ "Date: " ++ calendarTimeToString caltime ++ "\r\n"
       hPutStr h $ "Server: " ++ httpdVer ++ "\r\n"
       --hPutStr h "Connection: close\r\n"

processRequest :: Handle -> ParserState -> IO ()
processRequest h s = do 
    name <- realName "" segments
    case name of
              Just x -> IOE.try (openFile (fst x) ReadMode) >>= 
                        either (sendError h s . errCode) 
                                   (processMethod h s x)    
              Nothing -> sendError h s 404 -- XXX

    --IOE.try (openFile filename ReadMode) >>= 
    --    either (sendError h s . errCode) (processMethod h s filename)
    where
    segments = split "/" filename
    --realName :: String -> [String] -> IO (Maybe (String, String))
    realName l [] = return $ Just (l, "")
    realName l ("":xs) = realName l xs
    realName l (x:xs) = do 
                     dirExists <- doesDirectoryExist f
                     if dirExists == True
                        then realName f xs
                        else do fileExists <- doesFileExist f
                                if fileExists == True
                                   then return $ Just (f, concat $ intersperse "/" xs)
                                   else return $ Nothing
        where f = concat [l, "/", x]
    loc = takeWhile (/= '?') $ location s -- XXX
    filename = case loc of
               "/" -> path ++ "/index.html" -- XXX only append when it's a dir
               _ -> path ++ loc
    errCode e
        | IOE.isDoesNotExistError e = 404
        | IOE.isPermissionError e = 403
        | otherwise = 501

processMethod :: Handle -> ParserState -> (String, String)
              -> Handle -> IO ()
processMethod h s file h2 =
    case (contentType $ fst file) of
             "cgi-script" -> doCgi h s file
             _ -> case (method s) of
                           "GET" -> sendFile h s h2
                           "HEAD" -> sendHeaders h s h2
                           _ -> sendError h s 501 -- Not Implemented

split :: Eq a => [a] -> [a] -> [[a]]
split glue xs = split' xs
    where
    split' [] = []
    split' xs' = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs'
    dropGlue = drop (length glue)

breakOnGlue :: (Eq a) => [a]
            -> [a]
            -> ([a],[a])
breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest')
        where (piece, rest') = breakOnGlue glue xs

doCgi :: Handle -> ParserState -> (String, String) -> IO ()
doCgi h s f =
    do hPutStr h "HTTP/1.1 200 OK\r\n"
       sendCommonHeaders h
       hFlush h
       (stdinRd, stdinWr) <- createPipe
       stdinRdHd <- fdToHandle stdinRd
       stdinWrHd <- fdToHandle stdinWr

       ptr <- mallocBytes $ fromIntegral len
       hGetBuf h ptr $ fromIntegral len

       --hst <- (getHostByName $ clientHost s) >>=
       --       (inet_ntoa . head . hostAddresses) -- lame

       -- runProcess closes h
       putStrLn $ snd f
       id <- runProcess (fst f) args (Just dir) (Just $ concat 
                                                    [env "127.0.0.1", httpHeaders]) 
             (Just stdinRdHd) (Just h) Nothing

       hPutBuf stdinWrHd ptr $ fromIntegral len
       free ptr
       
       hClose stdinWrHd
       waitForProcess id
       return ()
    where 
    len
        | content_type == "" = 0::Integer
        | otherwise = read content_length
    dir = reverse $ tail $ dropWhile (/= '/') $ reverse $ fst f
    content_length = M.findWithDefault "" "Content-Length" $ headers s
    content_type = M.findWithDefault "" "Content-Type" $ headers s
    serverHost = takeWhile (/= ':') $ M.findWithDefault "" "Host" $ headers s
    serverPort = tail $ dropWhile (/= ':') $ M.findWithDefault "" "Host"
                 $ headers s
    httpHeaders = map (\(x, y) -> ("HTTP_" ++ map toUpper x, y)) $ M.toList
                  $ headers s
    queryString' = dropWhile (/= '?') $ location s
    queryString = if queryString' /= "" then tail queryString' else ""
    args = if '=' `elem` queryString then [] else split "+" queryString
    env hst = [("SERVER_SOFTWARE", httpdVer),
               ("SERVER_NAME", serverHost),
               ("GATEWAY_INTERFACE", "CGI/1.1"),
               ("SERVER_PROTOCOL", httpVer s),
               ("SERVER_PORT", serverPort),
               ("REQUEST_METHOD", method s),
               ("PATH_INFO", "/" ++ snd f), -- XXX
               ("PATH_TRANSLATED", fst f),
               ("SCRIPT_NAME", takeWhile (/= '?') $ location s),
               ("QUERY_STRING", queryString),
               ("REMOTE_ADDR", hst), -- XXX REMOTE_HOST too
               ("CONTENT_TYPE", content_type),
               ("CONTENT_LENGTH", content_length)]

sendHeaders :: Handle -> ParserState -> Handle -> IO ()
sendHeaders h s h2 = do
  size <- hFileSize h2
  hPutStr h "HTTP/1.1 200 OK\r\n"
  sendCommonHeaders h
  hPutStr h $ "Content-Length: " ++ (show size) ++ "\r\n"
  hPutStr h $ "Content-Type: " ++ contentType (location s) ++ "\r\n"
  hPutStr h "\r\n"

contentType :: String -> String
contentType s = case (getSuffix) of
                    "jpg" -> "image/JPEG"
                    "pdf" -> "application/pdf"
                    "cgi" -> "cgi-script"
                    "pl" ->  "cgi-script"
                    _ -> "text/html"
    where getSuffix = reverse $ takeWhile (not . (==) '.') $ reverse s

sendFile :: Handle -> ParserState -> Handle -> IO()
sendFile h s h2 = do
  size <- hFileSize h2
  allocaBytes (fromIntegral size) $ \ptr -> do
    hGetBuf h2 ptr $ fromIntegral size
    sendHeaders h s h2
    hPutBuf h ptr $ fromIntegral size

sendError :: Handle -> ParserState -> Int -> IO ()
sendError h s n =
    do hPutStr h $ "HTTP/1.1 " ++ (show n) ++ " Not Found\r\n" -- XXX
       sendCommonHeaders h
       hPutStr h "Content-Type: text/html; charset=iso-8859-1\r\n"
       hPutStr h "\r\n"
       hPutStr h $ "ERROR " ++ (show n) ++ "\r\n"
       hClose h

processConnection :: Handle -> Parser ()
processConnection h =
    do txt <- lift $ catch (hGetLine h) (const $ return "")
       case txt of
              "\r" -> do s <- get
                         lift $ processRequest h s
                         -- TODO logging
                         (lift $ IOE.try $ hFlush h) >>=
                              either (const $ return ()) (const $ keepAlive s)
                             where
                             keepAlive s = 
                                 case (M.findWithDefault ""
                                            "Connection"(headers s)) of
                                          "close" -> lift $ hClose h
                                          "keep-alive" -> do put emptyState
                                                             processConnection h
                                          _ -> lift $ hClose h
              "" -> return ()
              _ -> do parseHttp txt
                      s <- get
                      if (err s /= 0)
                         then lift $ sendError h s $ err s
                         else processConnection h

mainLoop :: Socket -> IO ()
mainLoop sock =
    do (handle, host, port) <- Network.accept sock
       forkOS $ evalStateT (processConnection handle)
                  $ emptyState { clientHost = host }
       mainLoop sock

main =
    do sock <- listenOn $ PortNumber port
       mainLoop sock
