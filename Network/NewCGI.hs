-----------------------------------------------------------------------------
-- |
-- Module      :  Network.NewCGI
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Bjorn Bringert 2004-2006
--                (c) Ian Lynagh 2005
--                (c) Jeremy Shaw 2005
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.State)
--
-- Simple Library for writing CGI programs.
-- See <http://hoohoo.ncsa.uiuc.edu/cgi/interface.html> for the
-- CGI specification.
--
-- Based on the original Haskell binding for CGI:
--
-- Original Version by Erik Meijer <mailto:erik@cs.ruu.nl>.
-- Further hacked on by Sven Panne <mailto:sven.panne@aedion.de>.
-- Further hacking by Andy Gill <mailto:andy@galconn.com>.
-- A new, hopefully more flexible, interface
-- and support for file uploads by Bjorn Bringert <mailto:bjorn@bringert.net>.
--
-- Here is a simple example, including error handling (not that there is 
-- much that can go wrong with Hello World):
--
-- > import Network.NewCGI
-- >
-- > cgiMain :: CGI CGIResult
-- > cgiMain = output "Hello World!"
-- >
-- > main :: IO ()
-- > main = runCGI (handleErrors cgiMain)
--
--
-----------------------------------------------------------------------------

module Network.NewCGI (
  -- * CGI monad
    MonadCGI, CGIT, CGIResult, CGI
  , MonadIO, liftIO
  , runCGI
  -- * Error handling
  , throwCGI, catchCGI, tryCGI, handleExceptionCGI
  , handleErrors
  -- * Logging
  , logCGI
  -- * Output
  , output, outputFPS, outputNothing, redirect
  , setHeader, setStatus
  -- * Error pages
  , outputError 
  , outputNotFound, outputMethodNotAllowed, outputInternalServerError
  -- * Input
  , getInput, getInputFPS, readInput
  , getInputs, getInputNames
  , getMultiInput
  , getInputFilename
  , getVar, getVars
  -- * Cookies
  , Cookie(..), newCookie
  , getCookie, readCookie
  , setCookie, deleteCookie
  -- * URL encoding
  , formEncode, urlEncode, formDecode, urlDecode
  -- * Compatibility with the old API
  , Html, wrapper, pwrapper, connectToCGIScript
  ) where

import Control.Exception (Exception(..))
import Control.Monad (liftM, unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (intersperse, sort, group)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.IO (Handle, hPutStrLn, stdin, stdout)
import System.IO.Error (isUserError, ioeGetErrorString)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

import Network.HTTP.Cookie (Cookie(..), showCookie, newCookie, findCookie)
import qualified Network.HTTP.Cookie as Cookie (deleteCookie)
import Network.NewCGI.Internals

-- imports only needed by the compatibility functions
import Control.Concurrent (forkIO)
import Control.Exception as Exception (Exception,throw,catch,finally)
import Network (PortID, Socket, listenOn, connectTo)
import Network.Socket as Socket (SockAddr(SockAddrInet), accept, socketToHandle)
import System.IO (hGetLine, hClose, IOMode(ReadWriteMode))
import System.IO.Error (isEOFError)
import Text.XHtml (Html, renderHtml, header, (<<), thetitle, (+++), 
                   body, h1, paragraph, hr, address)


-- | Run a CGI action. Typically called by the main function.
--   Reads input from stdin and writes to stdout. Gets
--   CGI environment variables from the program environment.
runCGI :: MonadIO m => CGIT m CGIResult -> m ()
runCGI = hRunCGI stdin stdout


--
-- * Output \/ redirect
--

-- | Output a 'String'. The output is assumed to be text\/html, encoded using
--   ISO-8859-1. To change this, set the Content-type header using
--   'setHeader'.
output :: MonadCGI m =>
          String        -- ^ The string to output.
       -> m CGIResult
output = return . CGIOutput . BS.pack

-- | Output a 'ByteString'. The output is assumed to be text\/html, 
--   encoded using ISO-8859-1. To change this, set the 
--   Content-type header using 'setHeader'.
outputFPS :: MonadCGI m =>
             ByteString        -- ^ The string to output.
          -> m CGIResult
outputFPS = return . CGIOutput

-- | Do not output anything (except headers).
outputNothing :: MonadCGI m => m CGIResult
outputNothing = return CGINothing

-- | Redirect to some location.
redirect :: MonadCGI m =>
            String        -- ^ A URL to redirect to.
         -> m CGIResult
redirect url = do setHeader "Location" url
                  outputNothing

--
-- * Error handling
--

-- | Catches any exception thrown by the given CGI action,
--   returns an error page with a 500 Internal Server Error,
--   showing the exception information, and logs the error.
--   
--   Typical usage:
--
-- > cgiMain :: CGI CGIResult
-- > cgiMain = ...
-- >
-- > main :: IO ()
-- > main = runCGI (handleErrors cgiMain)
handleErrors :: CGI CGIResult -> CGI CGIResult
handleErrors = flip catchCGI outputException

--
-- * Error output
--

-- | Output a 500 Internal Server Error with information from
--   an 'Exception'.
outputException :: (MonadCGI m,MonadIO m) => Exception -> m CGIResult
outputException e = outputInternalServerError es
    where es = case e of
                 ErrorCall msg  -> [msg]
                 IOException ie -> ioe ie
                 _              -> [show e]
          ioe ie = if isUserError ie then [ioeGetErrorString ie] else [show ie]

-- | Output an error page to the user, with the given
--   HTTP status code in the response. Also logs the error information
--   using 'logCGI'.
outputError :: (MonadCGI m, MonadIO m) =>
               Int      -- ^ HTTP Status code
            -> String   -- ^ Status message
            -> [String] -- ^ Error information
            -> m CGIResult
outputError c m es = 
      do logCGI $ show (c,m,es)
         setStatus c m
         setHeader "Content-type" "text/html; charset=ISO-8859-1"
         page <- errorPage c m es 
         output $ renderHtml page

-- | Create an HTML error page.
errorPage :: MonadCGI m => 
             Int      -- ^ Status code
          -> String   -- ^ Status message
          -> [String] -- ^ Error information
          -> m Html
errorPage c m es = 
    do server <- getVar "SERVER_SOFTWARE"
       host   <- getVar "SERVER_NAME"
       port   <- getVar "SERVER_PORT"
       let tit = show c ++ " " ++ m
           sig = "Haskell CGI" 
                 ++ " on " ++ fromMaybe "" server
                 ++ " at " ++ fromMaybe "" host ++ maybe "" (", port "++) port
       return $ header << thetitle << tit 
                  +++ body << (h1 << tit +++ map (paragraph <<) es 
                               +++ hr +++ address << sig)

--
-- * Specific HTTP errors
--

-- | Use 'outputError' to output and log a 404 Not Found error.
outputNotFound :: (MonadIO m, MonadCGI m) => 
                 String -- ^ The name of the requested resource.
              -> m CGIResult
outputNotFound r =
    outputError 404 "Not Found" ["The requested resource was not found: " ++ r]

-- | Use 'outputError' to output and log a 405 Method Not Allowed error.
outputMethodNotAllowed :: (MonadIO m, MonadCGI m) => 
                          [String] -- ^ The allowed methods.
                       -> m CGIResult
outputMethodNotAllowed ms = 
    do let allow = concat $ intersperse ", " ms
       setHeader "Allow" allow
       outputError 405 "Method Not Allowed" ["Allowed methods : " ++ allow]

-- | Use 'outputError' to output and log a 500 Internal Server Error.
outputInternalServerError :: (MonadIO m, MonadCGI m) =>
                             [String] -- ^ Error information.
                          -> m CGIResult
outputInternalServerError es = outputError 500 "Internal Server Error" es


--
-- * Environment variables
--

-- | Get the value of a CGI environment variable. Example:
--
-- > remoteAddr <- getVar "REMOTE_ADDR"
getVar :: MonadCGI m =>
          String             -- ^ The name of the variable.
       -> m (Maybe String)
getVar name = liftM (Map.lookup name) $ cgiGet cgiVars

-- | Get all CGI environment variables and their values.
getVars :: MonadCGI m =>
           m [(String,String)]
getVars = liftM Map.toList $ cgiGet cgiVars


--
-- * Inputs
--

-- | Get the value of an input variable, for example from a form.
--   If the variable has multiple values, the first one is returned.
--   Example:
--
-- > query <- getInput "query"
getInput :: MonadCGI m =>
            String           -- ^ The name of the variable.
         -> m (Maybe String) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInput = liftM (fmap inputValue) . getInput_

-- | Like 'getInput', but returns a 'ByteString'.
getInputFPS :: MonadCGI m =>
            String           -- ^ The name of the variable.
         -> m (Maybe ByteString) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInputFPS = liftM (fmap value) . getInput_

-- | Get all the values of an input variable, for example from a form.
-- This can be used to get all the values from form controls
-- which allow multiple values to be selected.
-- Example:
--
-- > vals <- getMultiInput "my_checkboxes"
getMultiInput :: MonadCGI m => 
                 String -- ^ The name of the variable.
              -> m [String] -- ^ The values of the variable,
                            -- or the empty list if the variable was not set.
getMultiInput n = do is <- cgiGet cgiInput
                     return [inputValue v | (p,v) <- is, p == n]

-- | Get the file name of an input.
getInputFilename :: MonadCGI m =>
                    String           -- ^ The name of the variable.
                 -> m (Maybe String) -- ^ The file name corresponding to the
                                     -- input, if there is one.
getInputFilename = liftM (>>= filename) . getInput_

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: (Read a, MonadCGI m) =>
             String        -- ^ The name of the variable.
          -> m (Maybe a) -- ^ 'Nothing' if the variable does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readInput = liftM (>>= maybeRead) . getInput

-- | Get the names and values of all inputs.
--   Note: the same name may occur more than once in the output,
--   if there are several values for the name.
getInputs :: MonadCGI m => m [(String,String)]
getInputs = do is <- cgiGet cgiInput
               return [ (n,inputValue i) | (n,i) <- is ]

-- | Get the names of all input variables.
getInputNames :: MonadCGI m => m [String]
getInputNames = (sortNub . map fst) `liftM` cgiGet cgiInput
    where sortNub = map head . group . sort

-- Internal stuff

inputValue :: Input -> String
inputValue = BS.unpack . value

getInput_ ::  MonadCGI m => String -> m (Maybe Input)
getInput_ n = lookup n `liftM` cgiGet cgiInput


--
-- * Cookies
--

-- | Get the value of a cookie.
getCookie :: MonadCGI m =>
             String           -- ^ The name of the cookie.
          -> m (Maybe String) -- ^ 'Nothing' if the cookie does not exist.
getCookie name = liftM (>>= findCookie name) (getVar "HTTP_COOKIE")

-- | Same as 'getCookie', but tries to read the value to the desired type.
readCookie :: (Read a, MonadCGI m) =>
              String       -- ^ The name of the cookie.
            -> m (Maybe a) -- ^ 'Nothing' if the cookie does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readCookie = liftM (>>= maybeRead) . getCookie

-- | Set a cookie.
setCookie :: MonadCGI m => Cookie -> m ()
setCookie = setHeader "Set-cookie" . showCookie

-- | Delete a cookie from the client
deleteCookie :: MonadCGI m => Cookie -> m ()
deleteCookie = setCookie . Cookie.deleteCookie


--
-- * Headers
--

-- | Set a response header.
--   Example:
--
-- > setHeader "Content-type" "text/plain"
setHeader :: MonadCGI m =>
             String -- ^ Header name.
          -> String -- ^ Header value.
          -> m ()
setHeader n v = cgiModify (\s -> s { cgiHeaders = f (cgiHeaders s) })
    where f = Map.insert (HeaderName n) v

-- | Set the HTTP response status.
setStatus :: MonadCGI m =>
             Int -- ^  HTTP status code, e.g. @404@
          -> String -- ^ HTTP status message, e.g. @"Not Found"@
          -> m ()
setStatus c m = setHeader "Status" (show c ++ " " ++ m)

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
     = do env <- getCGIVars
          input <- BS.hGetContents stdin
          let str = getRequestInput env input
          h <- connectTo host portId
                 `Exception.catch`
                   (\ e -> abort "Cannot connect to CGI daemon." e)
          BS.hPut h str >> hPutStrLn h ""
          (sendBack h `finally` hClose h)
               `Prelude.catch` (\e -> unless (isEOFError e) (ioError e))

-- | Returns the query string, or the request body if it is
--   a POST request, or the empty string if there is an error.
getRequestInput :: [(String,String)] -- ^ CGI environment variables.
                -> ByteString            -- ^ Request body.
                -> ByteString            -- ^ Query string.
getRequestInput env req =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> takeInput env req
      _ -> maybe BS.empty BS.pack (lookup "QUERY_STRING" env)

abort :: String -> Exception -> IO a
abort msg e =
    do putStrLn ("Content-type: text/html\n\n" ++
                   "<html><body>" ++ msg ++ "</body></html>")
       throw e

sendBack :: Handle -> IO ()
sendBack h = do s <- hGetLine h
                putStrLn s
                sendBack h
