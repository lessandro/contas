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
import System.Posix.Types
import System.Locale
import System.Posix.Signals
import System.Posix.User

path = "/Users/lzm/Sites/"
listenAddr = "0.0.0.0"
port = 1234
user = 501
httpdVer = "rephttpd/0.4"

data ParserState = ParserState {
    method :: String,
    location :: String,
    headers :: M.Map String String,
    httpVer :: String,
    err :: Int,
    clientHost :: String
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
              v = filter (/= '\r') $ concat $ intersperse " " ys 

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

processRequest :: Handle -> ParserState -> IO Integer
processRequest h s = do 
    name <- realName "" [""] segments
    case name of
              Just x -> do t <- IOE.try (openFile (fst x) ReadMode)
                           case t of
                               Left e -> do sendError h s (errCode e)
                                            return (errCode e)
                               Right f -> processMethod h s x f >> return 200
              Nothing -> sendError h s 404 >> (return 404) -- XXX
    where
    segments = split "/" filename
    realName :: String -> [String] -> [String] -> IO (Maybe (String, (String, String)))
    realName l scr [] = return $ Just (l, ("", concat $ intersperse "/" scr))
    realName l scr ("":xs) = realName l scr xs
    realName l scr ("..":xs) = realName l scr xs -- No directory traversal!
    realName l scr (x:xs) = do
                     dirExists <- doesDirectoryExist f
                     if dirExists == True
                        then realName f s' xs
                        else do fileExists <- doesFileExist f
                                if fileExists == True
                                   then return $ Just (f, (concat 
                                                      $ intersperse "/" xs,
                                                           concat $ intersperse "/" $ drop (length $ split "/" path) s'))
                                   else return $ Nothing
        where f = concat [l, "/", x]
              s' = scr ++ [x]
    loc = takeWhile (/= '?') $ location s -- XXX
    filename
        | loc ==  "/" = path ++ "/index.html" -- XXX only append when it's a dir
        -- | loc == "/tech" = path ++ "/" ++ loc ++ "/index.html" -- XXX
        | loc == "/tech" = path ++ "/" ++ loc ++ "/redir.html" -- XXX
        | last loc == '/' = path ++ "/" ++ loc ++ "index.html"
        | otherwise = path ++ loc
    errCode e
        | IOE.isDoesNotExistError e = 404
        | IOE.isPermissionError e = 403
        | otherwise = 501

processMethod :: Handle -> ParserState -> (String, (String, String))
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

getCgiHeaders :: Handle -> IO (M.Map String String)
getCgiHeaders h = do (_, s) <- runStateT (getCgiHeaders' h) emptyState
                     return $ headers s
    where 
    getCgiHeaders' :: Handle -> Parser ()
    getCgiHeaders' h = do
                  txt <- lift $ catch (hGetLine h) (const $ return "")
                  case txt of
                             "\r" -> return ()
                             "" -> return ()
                             _ -> do parseHttp txt
                                     s <- get
                                     if (err s /= 0)
                                        then lift $ sendError h s (err s) 
                                                 >> return ()
                                        else getCgiHeaders' h

doCgi :: Handle -> ParserState -> (String, (String, String)) -> IO ()
doCgi h s f =
    do (stdinRd, stdinWr) <- createPipe
       stdinRdHd <- fdToHandle stdinRd
       stdinWrHd <- fdToHandle stdinWr

       (stdoutRd, stdoutWr) <- createPipe
       stdoutRdHd <- fdToHandle stdoutRd
       stdoutWrHd <- fdToHandle stdoutWr

       ptr <- mallocBytes $ fromIntegral len
       hGetBuf h ptr $ fromIntegral len

       -- runProcess closes h
       id <- runProcess (fst f) args (Just dir) (Just $ filter ((/=) "" . snd) $ concat 
                                                    [env, httpHeaders]) 
             (Just stdinRdHd) (Just stdoutWrHd) Nothing

       hPutBuf stdinWrHd ptr $ fromIntegral len
       hClose stdinWrHd
       free ptr
       headers <- getCgiHeaders stdoutRdHd
       case M.findWithDefault "" "Status" headers of
                "" -> hPutStr h "HTTP/1.1 200 OK\r\n"
                m -> hPutStr h $ "HTTP/1.1 " ++ m ++ "\r\n"
       sendCommonHeaders h

       -- We could/should remove the Status: header here.
       mapM (\(k,v) -> hPutStr h $ k ++ ": " ++ v ++ "\r\n") $ M.toList headers
       hPutStr h "\r\n"
       hFlush h

       c <- catch (hGetContents stdoutRdHd) (const $ return "")
       hPutStr h c

       hClose stdoutRdHd
       hClose stdoutWrHd
       hClose h
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
    serverPort = case split ":" $ M.findWithDefault "" "Host" $ headers s of
                                                            [x] -> "80"
                                                            x -> last x
    httpHeaders = map (\(x, y) -> ("HTTP_" ++ map toUpper x, y)) $ M.toList
                  $ headers s
    queryString' = dropWhile (/= '?') $ location s
    queryString = if queryString' /= "" then tail queryString' else ""
    args = if '=' `elem` queryString then [] else split "+" queryString
    path_info = fst $ snd f
    env = [("SERVER_SOFTWARE", httpdVer),
          ("SERVER_NAME", serverHost),
          ("GATEWAY_INTERFACE", "CGI/1.1"),
          ("SERVER_PROTOCOL", httpVer s),
          ("SERVER_PORT", serverPort),
          ("REQUEST_METHOD", method s),
          ("PATH_INFO", case path_info of; "" -> ""; _ -> "/" ++ path_info),
          ("PATH_TRANSLATED", fst f),
          ("SCRIPT_NAME", "/" ++ (snd $ snd f)),
          ("QUERY_STRING", queryString),
          ("REMOTE_ADDR", clientHost s), -- XXX REMOTE_HOST too
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
                    "gif" -> "image/gif"
                    "png" -> "image/png"
                    "pdf" -> "application/pdf"
                    "tbz2" -> "application/x-bzip"
                    "cgi" -> "cgi-script"
                    "pl" ->  "cgi-script"
                    "css" -> "text/css"
                    "html" -> "text/html"
                    "htm" -> "text/html"
                    "bin" -> "application/octet-stream"
                    "cue" -> "application/octet-stream"
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

logRequest :: ParserState -> Integer -> IO ()
logRequest s code =
    do ctime <- getClockTime
       caltime <- toCalendarTime ctime
       putStrLn $ clientHost s ++ " " ++ formatCalendarTime defaultTimeLocale 
                    "[%D:%T]" caltime ++ " \"" ++ method s ++
                    " " ++ location s ++ " " ++ httpVer s ++ "\" " ++ show code
       hFlush stdout

processConnection :: Handle -> Parser ()
processConnection h =
    do txt <- lift $ catch (hGetLine h) (const $ return "")
       case txt of
              "\r" -> do s <- get
                         code <- lift $ processRequest h s
                      
                         lift $ logRequest s code

                         (lift $ IOE.try $ hFlush h) >>=
                              either (const $ return ()) (const $ keepAlive s)
                             where
                             keepAlive s = 
                                 case (M.findWithDefault ""
                                            "Connection"(headers s)) of
                                          "close" -> lift $ hClose h
                                          "keep-alive" -> do put emptyState { clientHost = clientHost s }
                                                             processConnection h
                                          _ -> lift $ hClose h
              "" -> return ()
              _ -> do parseHttp txt
                      s <- get
                      if (err s /= 0)
                         then do
                           lift $ sendError h s $ err s
                           lift $ logRequest s $ fromIntegral $ err s
                         else processConnection h

mainLoop :: Socket -> IO ()
mainLoop sock =
    do (s, saddr) <- Network.Socket.accept sock
       handle <- fdToHandle $ Fd $ fdSocket s
       addr <- inet_ntoa $ ip saddr
       forkIO $ evalStateT (processConnection handle)
                  $ emptyState { clientHost = addr }
       mainLoop sock
           where ip (SockAddrInet _ a) = a
main =
    do sock <- socket AF_INET Stream 0
       setSocketOption sock ReuseAddr 1
       addr <- inet_addr listenAddr
       bindSocket sock (SockAddrInet port addr)
       listen sock (-1)

       installHandler sigPIPE Ignore Nothing
       setUserID user
       mainLoop sock
