-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Bjorn Bringert 2004-2005
--                (c) Ian Lynagh 2005
-- License     :  BSD-style
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
    MonadCGI, CGIT, CGIResult, CGI
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
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO)
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
import System.IO.Error (isEOFError)
import Text.Html (Html, renderHtml)


data CGIState = CGIState {
                          cgiVars :: [(String,String)],
                          cgiInput :: [(String,String)],
                          cgiResponseHeaders :: [(String,String)]
                         }
              deriving (Show, Read, Eq, Ord)

-- | The CGIT monad transformer.
newtype CGIT m a = CGIT { unCGIT :: StateT CGIState m a }

-- | A simple CGI monad with just IO.
type CGI a = CGIT IO a

-- | The result of a CGI program.
data CGIResult = CGIOutput String
               | CGIRedirect String
                 deriving (Show, Read, Eq, Ord)

--
-- * CGIT monad transformer
--

instance Monad m => Monad (CGIT m) where
    c >>= f = CGIT (unCGIT c >>= unCGIT . f)
    return = CGIT . return
    -- FIXME: should we have an error monad instead?
    fail s = CGIT (fail s)

instance MonadIO m => MonadIO (CGIT m) where
    liftIO f = CGIT (liftIO f)

class Monad m => MonadCGI m where
    -- | Modify the CGIT state.
    cgiModify :: (CGIState -> CGIState) -> m ()
    -- | Get something from the CGIT state.
    cgiGet :: (CGIState -> a) -> m a

instance Monad m => MonadCGI (CGIT m) where
    cgiModify = CGIT . modify
    cgiGet = CGIT . gets

{-
-- requires -fallow-undecidable-instances and -fallow-overlapping-instances
instance (MonadTrans t, MonadCGI m, Monad (t m)) => MonadCGI (t m) where
    cgiModify f = lift (cgiModify f)
    cgiGet f = lift (cgiGet f)
-}

instance MonadTrans CGIT where
    lift = CGIT . lift


-- | Perform an IO action in the CGI monad.
--   This is just 'liftIO' specilized to the CGIT monad.
io :: MonadIO m => IO a -> CGIT m a
io = liftIO

-- | Run a CGI action. Typically called by the main function.
--   Reads input from stdin and writes to stdout. Gets
--   CGI environment variables from the program environment.
runCGI :: MonadIO m => CGIT m CGIResult -> m ()
runCGI = hRunCGI stdin stdout

-- | Run a CGI action. Gets CGI environment variables from
--   the program environment.
hRunCGI :: MonadIO m =>
	   Handle -- ^ Handle that input will be read from.
        -> Handle -- ^ Handle that output will be written to.
        -> CGIT m CGIResult -> m ()
hRunCGI hin hout f = do env <- liftIO getCgiVars
                        inp <- liftIO $ hGetContents hin
                        outp <- runCGIEnv env inp f
                        liftIO $ hPutStr hout outp
                        liftIO $ hFlush hout

-- | Run a CGI action in a given environment, using (lazy) strings
--   for input and output.
runCGIEnv :: Monad m =>
	     [(String,String)] -- ^ CGI environment variables.
          -> String -- ^ Request body.
          -> CGIT m CGIResult -- ^ CGI action.
          -> m String -- ^ Response (headers and content).
runCGIEnv vars inp f
    = do let qs = getQueryString vars inp
             s = CGIState {
                           cgiVars = vars,
                           cgiInput = formDecode qs,
                           cgiResponseHeaders = initHeaders
                          }
         (outp,s') <- runStateT (unCGIT f) s
         let hs = cgiResponseHeaders s'
         return $ case outp of
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
output :: MonadCGI m =>
	  String        -- ^ The string to output.
       -> m CGIResult
output = return . CGIOutput

-- | Redirect to some location.
redirect :: MonadCGI m =>
	    String        -- ^ A URL to redirect to.
         -> m CGIResult
redirect = return . CGIRedirect

--
-- * HTTP variables
--

-- | Get the value of a CGI environment variable. Example:
--
-- > remoteAddr <- getVar "REMOTE_ADDR"
getVar :: MonadCGI m =>
	  String             -- ^ The name of the variable.
       -> m (Maybe String)
getVar name = liftM (lookup name) getVars

-- | Get all CGI environment variables and their values.
getVars :: MonadCGI m =>
	   m [(String,String)]
getVars = cgiGet cgiVars

--
-- * Query input
--

-- | Get an input variable, for example from a form.
--   Example:
--
-- > query <- getInput "query"
getInput :: MonadCGI m =>
	    String             -- ^ The name of the variable.
         -> m (Maybe String) -- ^ The value of the variable,
                               --   or Nothing, if it was not set.
getInput name = lookup name `liftM` getInputs

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: (Read a, MonadCGI m) =>
             String        -- ^ The name of the variable.
          -> m (Maybe a) -- ^ 'Nothing' if the variable does not exist
                           --   or if the value could not be interpreted
                           --   as the desired type.
readInput name = maybeRead `inside` getInput name

-- | Get all input variables and their values.
getInputs :: MonadCGI m =>
	     m [(String,String)]
getInputs = cgiGet cgiInput

--
-- * Cookies
--

-- | Get the value of a cookie.
getCookie :: MonadCGI m =>
	     String             -- ^ The name of the cookie.
          -> m (Maybe String)
getCookie name = findCookie name `inside` getVar "HTTP_COOKIE"

-- | Set a cookie.
setCookie :: MonadCGI m => Cookie -> m ()
setCookie = modifyHeaders . Cookie.setCookie

-- | Delete a cookie from the client
deleteCookie :: MonadCGI m => Cookie -> m ()
deleteCookie = setCookie . Cookie.deleteCookie


--
-- * Headers
--

-- | Modify the response headers.
modifyHeaders :: MonadCGI m =>
		 ([(String,String)] -> [(String,String)])
	      -> m ()
modifyHeaders f = cgiModify (\s -> s{cgiResponseHeaders 
				     = f (cgiResponseHeaders s)})

-- | Set a response header.
--   Example:
--
-- > setHeader "Content-type" "text/plain"
setHeader :: MonadCGI m =>
	     String -- ^ Header name.
          -> String -- ^ Header value.
          -> m ()
setHeader name value = modifyHeaders (tableSet name value)

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

unlinesS :: [ShowS] -> ShowS
unlinesS = join "\n"

join :: String -> [ShowS] -> ShowS
join glue = concatS . intersperse (showString glue)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

inside :: (Monad m, Monad n) => (a -> n b) -> m (n a) -> m (n b)
inside = liftM . (=<<)

--
-- * CGI protocol stuff
--

getCgiVars :: IO [(String,String)]
getCgiVars = mapM (\n -> (,) n `liftM` getEnvOrNil n) cgiVarNames

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

getEnvOrNil :: String -> IO String
getEnvOrNil v = getEnv v `Prelude.catch` const (return "")

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
wrapper = runCGI . wrapCGI

-- | Compatibility wrapper for the old CGI interface.
--   Runs a simple CGI server.
--   Note: if using Windows, you might need to wrap 'withSocketsDo' around main.
pwrapper :: PortID  -- ^ The port to run the server on.
         -> ([(String,String)] -> IO Html)
         -> IO ()
pwrapper pid f = do sock <- listenOn pid
                    acceptConnections fn sock
 where fn h = hRunCGI h h (wrapCGI f)

acceptConnections :: (Handle -> IO ()) -> Socket -> IO ()
acceptConnections fn sock = do
  (h, SockAddrInet _ _) <- accept' sock
  forkIO (fn h `finally` (hClose h))
  acceptConnections fn sock

accept' :: Socket                 -- Listening Socket
       -> IO (Handle,SockAddr)        -- StdIO Handle for read/write
accept' sock = do
 (sock', addr) <- Socket.accept sock
 handle        <- socketToHandle sock' ReadWriteMode
 return (handle,addr)

wrapCGI :: MonadIO m => ([(String,String)] -> IO Html) -> CGIT m CGIResult
wrapCGI f = do
            vs <- getVars
            is <- getInputs
            html <- liftIO (f (vs++is))
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

sendBack :: Handle -> IO ()
sendBack h = do s <- hGetLine h
                putStrLn s
                sendBack h
