-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Bjorn Bringert 2004-2005
-- License     :  BSD-style (see the file libraries/network/LICENSE)
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.State)
--
-- Simple library for writing CGI programs.
--
-- Based on the original Haskell binding for CGI:
--
-- Original Version by Erik Meijer <mailto:erik@cs.ruu.nl>.
-- Further hacked on by Sven Panne <mailto:sven.panne@aedion.de>.
-- Further hacking by Andy Gill <mailto:andy@galconn.com>.
--
-----------------------------------------------------------------------------

module Network.NewCGI (
  -- * The CGI monad
    CGI, CGIResult
  , io
  , runCGI, hRunCGI, runCGIEnv
  -- * Output
  , output, redirect
  , setHeader
  -- * Input
  , getInput, readInput, getInputs
  , getVar, getVars
  -- * Cookies
  , Cookie(..), newCookie
  , getCookie, setCookie, deleteCookie
  -- * Compatibility
  , Html, wrapper, pwrapper, connectToCGIScript
  ) where

import Control.Monad (liftM, unless)
import Control.Monad.State (StateT, gets, lift, modify, runStateT)
import Data.List (intersperse)
import Data.Maybe (listToMaybe, fromMaybe)
import Network.HTTP.Cookie (Cookie(..), newCookie, findCookie)
import qualified Network.HTTP.Cookie as Cookie (setCookie, deleteCookie)
import Network.URI (unEscapeString)
import System.Environment (getEnv)
import System.IO (Handle, hPutStr, hPutStrLn, hGetContents, 
                  stdin, stdout, hFlush)

-- imports only needed by the compatibility functions
import Control.Concurrent (forkIO)
import Control.Exception as Exception (Exception,throw,catch,finally)
import Network (PortID, Socket, listenOn, connectTo)
import Network.Socket as Socket (SockAddr(SockAddrInet), accept, socketToHandle)
import System.IO (hGetLine, hClose, IOMode(ReadWriteMode))
import System.IO.Error (catch, isEOFError)
import Text.Html (Html, renderHtml)


data CGIState = CGIState {
                          cgiVars :: [(String,String)],
                          cgiInput :: [(String,String)],
                          cgiResponseHeaders :: [(String,String)]
                         }
              deriving (Show, Read, Eq, Ord)

-- | The CGI monad.
newtype CGI a = CGI { unCGI :: StateT CGIState IO a }

-- | The result of a CGI program.
data CGIResult = CGIOutput String
               | CGIRedirect String
                 deriving (Show, Read, Eq, Ord)

--
-- * CGI monad
--

instance Monad CGI where
    c >>= f = CGI (unCGI c >>= unCGI . f)
    return = CGI . return
    -- FIXME: should we have an error monad instead?
    fail s = io (fail s)

-- | Perform an IO action in the CGI monad.
io :: IO a -> CGI a
io = CGI . lift

-- | Get something from the CGI state.
cgiGet :: (CGIState -> a) -> CGI a
cgiGet = CGI . gets

-- | Modify the CGI state.
cgiModify :: (CGIState -> CGIState) -> CGI ()
cgiModify = CGI . modify

-- | Run a CGI action. Typically called by the main function.
--   Reads input from stdin and writes to stdout. Gets
--   CGI environment variables from the program environment.
runCGI :: CGI CGIResult -> IO ()
runCGI = hRunCGI stdin stdout

-- | Run a CGI action. Gets CGI environment variables from 
--   the program environment.
hRunCGI :: Handle -- ^ Handle that input will be read from.
        -> Handle -- ^ Handle that output will be written to.
        -> CGI CGIResult -> IO ()
hRunCGI hin hout f = do env <- getCgiVars
                        input <- hGetContents hin
                        output <- runCGIEnv env input f
                        hPutStr hout output
                        hFlush hout

-- | Run a CGI action in a given environment, using (lazy) strings 
--   for input and output. 
runCGIEnv :: [(String,String)] -- ^ CGI environment variables.
          -> String -- ^ Request body.
          -> CGI CGIResult -- ^ CGI action.
          -> IO String -- ^ Response (headers and content).
runCGIEnv vars input f 
    = do let qs = getQueryString vars input
             s = CGIState {
                           cgiVars = vars,
                           cgiInput = formDecode qs,
                           cgiResponseHeaders = initHeaders
                          }
         (output,s') <- runStateT (unCGI f) s
         let hs = cgiResponseHeaders s'
         return $ case output of
           CGIOutput str   -> formatResponse str hs'
               where hs' = tableAddIfNotPresent "Content-type" defaultContentType hs
           CGIRedirect url -> formatResponse "" hs'
               where hs' = tableSet "Location" url hs

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

formatResponse :: String -> [(String,String)]-> String
formatResponse c hs = unlinesS (map showHeader hs ++ [id, showString c]) ""

--
-- * Output \/ redirect
--

-- | Output a string. The output is assumed to be text\/html, encoded using
--   ISO-8859-1. To change this, set the Content-type header using
--   'setHeader'.
output :: String        -- ^ The string to output.
       -> CGI CGIResult
output str = return $ CGIOutput str

-- | Redirect to some location.
redirect :: String        -- ^ A URL to redirect to.
         -> CGI CGIResult
redirect str = return $ CGIRedirect str

--
-- * HTTP variables
--

-- | Get the value of a CGI environment variable. Example:
--
-- > remoteAddr <- getVar "REMOTE_ADDR"
getVar :: String             -- ^ The name of the variable.
       -> CGI (Maybe String)
getVar name = liftM (lookup name) getVars

-- | Get all CGI environment variables and their values.
getVars :: CGI [(String,String)]
getVars = cgiGet cgiVars

--
-- * Query input
--

-- | Get an input variable, for example from a form.
--   Example:
--
-- > query <- getInput "query"
getInput :: String             -- ^ The name of the variable.
         -> CGI (Maybe String) -- ^ The value of the variable,
                               --   or Nothing, if it was not set.
getInput name = liftM (lookup name) getInputs

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: Read a => 
             String        -- ^ The name of the variable.
          -> CGI (Maybe a) -- ^ 'Nothing' if the variable does not exist
                           --   or if the value could not be interpreted
                           --   as the desired type.
readInput name = liftM (>>= maybeRead) (getInput name)

-- | Get all input variables and their values.
getInputs :: CGI [(String,String)]
getInputs = cgiGet cgiInput

--
-- * Cookies
--

-- | Get the value of a cookie.
getCookie :: String             -- ^ The name of the cookie.
          -> CGI (Maybe String)
getCookie name = do
                 cs <- getVar "HTTP_COOKIE"
                 return $ maybe Nothing (findCookie name) cs

-- | Set a cookie.
setCookie :: Cookie -> CGI ()
setCookie cookie = 
    cgiModify (\s -> s{cgiResponseHeaders 
                    = Cookie.setCookie cookie (cgiResponseHeaders s)})

-- | Delete a cookie from the client
deleteCookie :: Cookie -> CGI ()
deleteCookie cookie = setCookie (Cookie.deleteCookie cookie)


--
-- * Headers
--

-- | Set a response header.
--   Example:
--
-- > setHeader "Content-type" "text/plain"
setHeader :: String -- ^ Header name.
          -> String -- ^ Header value.
          -> CGI ()
setHeader name value = 
    cgiModify (\s -> s{cgiResponseHeaders 
                    = tableSet name value (cgiResponseHeaders s)})

showHeader :: (String,String) -> ShowS
showHeader (n,v) = showString n . showString ": " . showString v

initHeaders :: [(String,String)]
initHeaders = []

--
-- * Utilities
--

-- | Get the name-value pairs from application\/x-www-form-urlencoded data.
formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
    where (nv,rs) = break (=='&') s
          (n,v) = break (=='=') nv

-- | Convert a single value from the application\/x-www-form-urlencoded encoding.
urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '

-- | Replace all instances of a value in a list by another value.
replace :: Eq a => 
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

-- | Set a value in a lookup table.
tableSet :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
tableSet k v [] = [(k,v)]
tableSet k v ((k',v'):ts) 
    | k == k' = (k,v) : ts
    | otherwise = (k',v') : tableSet k v ts

-- | Add a key, value pair to a table only if there is no entry
--   with the given key already in the table. If there is an entry
--   already, nothing is done.
tableAddIfNotPresent :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
tableAddIfNotPresent k v [] = [(k,v)]
tableAddIfNotPresent k v ((k',v'):ts) 
    | k == k' = (k',v') : ts
    | otherwise = (k',v') : tableAddIfNotPresent k v ts

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

unwordsS :: [ShowS] -> ShowS
unwordsS = join " "

unlinesS :: [ShowS] -> ShowS
unlinesS = join "\n"

join :: String -> [ShowS] -> ShowS
join glue = concatS . intersperse (showString glue)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

maybeToM :: Monad m => String -> Maybe a -> m a
maybeToM err = maybe (fail err) return

--
-- * CGI protocol stuff
--

getCgiVars :: IO [(String,String)]
getCgiVars = do vals <- mapM myGetEnv cgiVarNames
                return (zip cgiVarNames vals)

cgiVarNames :: [String]
cgiVarNames =
   [ "DOCUMENT_ROOT"
   , "AUTH_TYPE"
   , "GATEWAY_INTERFACE"
   , "SERVER_SOFTWARE"
   , "SERVER_NAME"
   , "REQUEST_METHOD"
   , "SERVER_ADMIN"
   , "SERVER_PORT"
   , "QUERY_STRING"
   , "CONTENT_LENGTH"
   , "CONTENT_TYPE"
   , "REMOTE_USER"
   , "REMOTE_IDENT"
   , "REMOTE_ADDR"
   , "REMOTE_HOST"
   , "TZ"
   , "PATH"
   , "PATH_INFO"
   , "PATH_TRANSLATED"
   , "SCRIPT_NAME"
   , "SCRIPT_FILENAME"
   , "HTTP_COOKIE"
   , "HTTP_CONNECTION"
   , "HTTP_ACCEPT_LANGUAGE"
   , "HTTP_ACCEPT"
   , "HTTP_HOST"
   , "HTTP_UA_COLOR"
   , "HTTP_UA_CPU"
   , "HTTP_UA_OS"
   , "HTTP_UA_PIXELS"
   , "HTTP_USER_AGENT"
   ]                      

-- | Returns the query string, or the empty string if there is 
--   an error.
getQueryString :: [(String,String)] -- ^ CGI environment variables.
               -> String            -- ^ Request body.
               -> String            -- ^ Query string.
getQueryString env req =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> 
        let len = lookup "CONTENT_LENGTH" env >>= maybeRead
         in case len of
              -- FIXME: we should check that length req == len,
           -- but that would force evaluation of req
           Just l  -> take l req
           Nothing -> ""
      _ -> lookupOrNil "QUERY_STRING" env

myGetEnv :: String -> IO String
myGetEnv v = Prelude.catch (getEnv v) (const (return ""))

lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n

--
-- * Compatibility functions
--

{-# DEPRECATED wrapper, pwrapper, connectToCGIScript "Use the new interface." #-}

-- | Compatibility wrapper for the old CGI interface. 
--   Output the output from a function from CGI environment and 
--   input variables to an HTML document.
wrapper :: ([(String,String)] -> IO Html) -> IO ()
wrapper f = runCGI (wrapCGI f)

-- | Compatibility wrapper for the old CGI interface.
--   Runs a simple CGI server.
--   Note: if using Windows, you might need to wrap 'withSocketsDo' around main.
pwrapper :: PortID  -- ^ The port to run the server on.
         -> ([(String,String)] -> IO Html) 
         -> IO ()
pwrapper pid f = do sock <- listenOn pid
                    acceptConnections fn sock
 where fn h = hRunCGI h h (wrapCGI f)

acceptConnections fn sock = do
  (h, SockAddrInet port haddr) <- accept' sock
  forkIO (fn h `finally` (hClose h))
  acceptConnections fn sock

accept' :: Socket                 -- Listening Socket
       -> IO (Handle,SockAddr)        -- StdIO Handle for read/write
accept' sock = do
 (sock', addr) <- Socket.accept sock
 handle        <- socketToHandle sock' ReadWriteMode
 return (handle,addr)

wrapCGI :: ([(String,String)] -> IO Html) -> CGI CGIResult
wrapCGI f = do
            vs <- getVars
            is <- getInputs
            html <- io (f (vs++is))
            output (renderHtml html)

-- | Note: if using Windows, you might need to wrap 'withSocketsDo' around main.
connectToCGIScript :: String -> PortID -> IO ()
connectToCGIScript host portId
     = do env <- getCgiVars
          input <- getContents
          let str = getQueryString env input
          h <- connectTo host portId
                 `Exception.catch`
                   (\ e -> abort "Cannot connect to CGI daemon." e)
          hPutStrLn h str
          (sendBack h `finally` hClose h)
               `Prelude.catch` (\e -> unless (isEOFError e) (ioError e))

abort :: String -> Exception -> IO a
abort msg e = 
    do putStrLn ("Content-type: text/html\n\n" ++
                   "<html><body>" ++ msg ++ "</body></html>")
       throw e

sendBack h = do s <- hGetLine h
                putStrLn s
                sendBack h
