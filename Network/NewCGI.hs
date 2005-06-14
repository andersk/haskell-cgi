-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Bjorn Bringert 2004
-- License     :  BSD-style (see the file libraries/network/LICENSE)
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (uses Network.URI and Control.Monad.State)
--
-- Simple library for writing CGI programs.
--
-- Based on the original Haskell binding for CGI:
--
-- Original Version by Erik Meijer <mailto:erik@cs.ruu.nl>
-- Further hacked on by Sven Panne <mailto:sven.panne@aedion.de>
-- Further hacking by Andy Gill <mailto:andy@galconn.com>
--
-----------------------------------------------------------------------------

module Network.NewCGI (
			  CGI, CGIResult
			 , output, redirect
			 , io, getVar, 
			 , getInput, readInput, getInputs
			 , runCGI
			 , setHeader
			 , Cookie(..), newCookie
			 , getCookie, setCookie, deleteCookie
			 ) where

import Control.Monad.State
import Data.Maybe (listToMaybe)
import Network.HTTP.Cookie (Cookie(..), newCookie, findCookie)
import qualified Network.HTTP.Cookie as Cookie (setCookie, deleteCookie)
import Network.URI (unEscapeString)
import System.Environment (getEnv)

data CGIState = CGIState {
			  cgiVars :: [(String,String)],
			  cgiInput :: [(String,String)],
			  cgiResponseHeaders :: [(String,String)]
			 }
	      deriving (Show, Read, Eq, Ord)

type CGI a = StateT CGIState IO a 

data CGIResult = CGIOutput String
	       | CGIRedirect String
		 deriving (Show, Read, Eq, Ord)

--
-- * CGI monad
--

-- | Perform an IO action in the CGI monad
io :: IO a -> CGI a
io = lift

-- | Run a CGI action.
--   Note: if using Windows, you might need to wrap 'withSocketsDo' round main.
runCGI :: CGI CGIResult -> IO ()
runCGI f = do qs <- getQueryString
	      vars <- getCgiVars
	      let s = CGIState{
			       cgiVars = vars,
			       cgiInput = formDecode qs,
			       cgiResponseHeaders = initHeaders
			      }
  	      (output,s') <- runStateT f s
	      let hs = cgiResponseHeaders s'
	      case output of
			  CGIOutput str   -> doOutput str hs
			  CGIRedirect url -> doRedirect url hs


doOutput :: String -> [(String,String)] -> IO ()
doOutput str hs = 
    do
    let hs' = tableAddIfNotPresent "Content-type" "text/html; charset=ISO-8859-1" hs
    printHeaders hs'
    putStrLn ""
    putStr str

doRedirect :: String -> [(String,String)] -> IO ()
doRedirect url hs =
    do
    let hs' = tableSet "Location" url hs
    printHeaders hs'
    putStrLn ""

--
-- * Output / redirect
--

-- | Output a string.
output :: String -> CGI CGIResult
output str = return $ CGIOutput str

-- | Redirect to some location.
redirect :: String -> CGI CGIResult
redirect str = return $ CGIRedirect str

--
-- * HTTP variables
--

-- | Get a CGI variable. Example:
--
-- > remoteAddr <- getVar "REMOTE_ADDR"
getVar :: String -> CGI (Maybe String)
getVar name = gets (lookup name . cgiVars)

--
-- * Query input
--

-- | Get an input variable, for example from a form.
--   Example:
--
-- > query <- getInput "query"
getInput :: String -> CGI (Maybe String)
getInput name = gets (lookup name . cgiInput)

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: Read a => 
	     String 
	  -> CGI (Maybe a) -- ^ 'Nothing' if the variable does not exist
	                   --   or if the value could not be interpreted
	                   --   as the desired type.
readInput name = liftM (>>= fmap fst . listToMaybe . reads) (getInput name)

-- | Get all input variables and their values.
getInputs :: CGI [(String,String)]
getInputs = gets cgiInput

--
-- * Cookies
--

-- | Get the value of a cookie
getCookie :: String -> CGI (Maybe String)
getCookie name = do
		 cs <- getVar "HTTP_COOKIE"
		 return $ maybe Nothing (findCookie name) cs

-- | Set a cookie
setCookie :: Cookie -> CGI ()
setCookie cookie = 
    modify (\s -> s{cgiResponseHeaders 
		    = Cookie.setCookie cookie (cgiResponseHeaders s)})

-- | Delete a cookie from the client
deleteCookie :: Cookie -> CGI ()
deleteCookie cookie = setCookie (Cookie.deleteCookie cookie)


--
-- * Headers
--

-- | Set a response header
setHeader :: String -> String -> CGI ()
setHeader name value = 
    modify (\s -> s{cgiResponseHeaders 
		    = tableSet name value (cgiResponseHeaders s)})

showHeader :: (String,String) -> String
showHeader (n,v) = n ++ ": " ++ v

printHeaders :: [(String,String)] -> IO ()
printHeaders = mapM_ (putStrLn . showHeader)

initHeaders :: [(String,String)]
initHeaders = []

--
-- * Utilities
--


-- | Get the name-value pairs from application/x-www-form-urlencoded data
formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
    where (nv,rs) = break (=='&') s
	  (n,v) = break (=='=') nv


-- | Convert a single value from the application/x-www-form-urlencoded encoding.
urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '


-- | Replace all instances of a value in a list by another value.
replace :: Eq a => 
	   a   -- ^ Value to look for
	-> a   -- ^ Value to replace it with
	-> [a] -- ^ Input list
	-> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)


tableSet :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
tableSet k v [] = [(k,v)]
tableSet k v ((k',v'):ts) 
    | k == k' = (k,v) : ts
    | otherwise = (k',v') : tableSet k v ts

tableAddIfNotPresent :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
tableAddIfNotPresent k v [] = [(k,v)]
tableAddIfNotPresent k v ((k',v'):ts) 
    | k == k' = (k',v') : ts
    | otherwise = (k',v') : tableAddIfNotPresent k v ts

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

getQueryString :: IO String
getQueryString = do
   method <- myGetEnv "REQUEST_METHOD"
   case method of
      "POST" -> do len <- myGetEnv "CONTENT_LENGTH"
                   inp <- getContents
                   return (take (read len) inp)
      _      -> myGetEnv "QUERY_STRING"

myGetEnv :: String -> IO String
myGetEnv v = Prelude.catch (getEnv v) (const (return ""))
