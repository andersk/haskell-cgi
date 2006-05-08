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
--
-- Based on the original Haskell binding for CGI:
--
-- Original Version by Erik Meijer <mailto:erik@cs.ruu.nl>.
-- Further hacked on by Sven Panne <mailto:sven.panne@aedion.de>.
-- Further hacking by Andy Gill <mailto:andy@galconn.com>.
-- A new, hopefully more flexible, interface
-- and support for file uploads by Bjorn Bringert <mailto:bjorn@bringert.net>.
--
-----------------------------------------------------------------------------

module Network.NewCGI (
  -- * The CGI monad
    MonadCGI (..), CGIState (..), CGIT (..), CGIResult, CGI
  , MonadIO, liftIO
  , runCGI, hRunCGI, runCGIEnv, runCGIEnvFPS
  -- * Error handling
  , handleExceptionCGI
  -- * Logging
  , logCGI
  -- * Output
  , output, outputFPS, redirect
  , setHeader
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
  -- * Compatibility
  , Html, wrapper, pwrapper, connectToCGIScript
  ) where

import Control.Exception as Exception (try)
import Control.Monad (liftM, unless)
import Control.Monad.State (StateT(..), gets, lift, modify)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO)
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import Network.HTTP.Cookie (Cookie(..), newCookie, findCookie)
import qualified Network.HTTP.Cookie as Cookie (setCookie, deleteCookie)
import Network.URI (unEscapeString)
import System.Environment (getEnv)
import System.IO (Handle, hPutStrLn,
                  stdin, stdout, stderr, hFlush)

import Network.Multipart
import qualified Data.FastPackedString as FPS
import Data.FastPackedString (FastString)


-- imports only needed by the compatibility functions
import Control.Concurrent (forkIO)
import Control.Exception as Exception (Exception,throw,catch,finally)
import Network (PortID, Socket, listenOn, connectTo)
import Network.Socket as Socket (SockAddr(SockAddrInet), accept, socketToHandle)
import System.IO (hGetLine, hClose, IOMode(ReadWriteMode))
import System.IO.Error (isEOFError)
import Text.Html (Html, renderHtml)


data CGIState = CGIState {
                          cgiVars :: Map String String,
                          cgiInput :: Map String [Input],
                          cgiResponseHeaders :: [(String,String)]
                         }
              deriving Show

data Input = Input {
                    value :: FastString,
                    filename :: Maybe String,
                    contentType :: ContentType
                   }
              deriving Show

-- | The CGIT monad transformer.
newtype CGIT m a = CGIT { unCGIT :: StateT CGIState m a }

-- | A simple CGI monad with just IO.
type CGI a = CGIT IO a

-- | The result of a CGI program.
data CGIResult = CGIOutput FastString
               | CGIRedirect String
                 deriving (Show, Read, Eq, Ord)

--
-- * CGIT monad transformer
--

instance Monad m => Functor (CGIT m) where
    fmap f c = CGIT (fmap f (unCGIT c))

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

instance MonadTrans CGIT where
    lift = CGIT . lift


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
                        inp <- liftIO $ FPS.hGetContents hin
                        outp <- runCGIEnvFPS env inp f
                        liftIO $ FPS.hPut hout outp
                        liftIO $ hFlush hout

-- | Run a CGI action in a given environment, using strings
--   for input and output. Note: this can be inefficient,
--   especially with file uploads. Use 'runCGIEnvFPS'
--   instead.
runCGIEnv :: Monad m =>
             [(String,String)] -- ^ CGI environment variables.
          -> String -- ^ Request body.
          -> CGIT m CGIResult -- ^ CGI action.
          -> m String -- ^ Response (headers and content).
runCGIEnv vars inp f = liftM FPS.unpack $ runCGIEnvFPS vars (FPS.pack inp) f

-- | Run a CGI action in a given environment, using a 'FastString'
--   for input and a lazy string for output. 
runCGIEnvFPS :: Monad m =>
             [(String,String)] -- ^ CGI environment variables.
          -> FastString -- ^ Request body.
          -> CGIT m CGIResult -- ^ CGI action.
          -> m FastString -- ^ Response (headers and content).
runCGIEnvFPS vars inp f
    = do let s = CGIState {
                           cgiVars = Map.fromList vars,
                           cgiInput = mkMultiMap $ decodeInput vars inp,
                           cgiResponseHeaders = initHeaders
                          }
         (outp,s') <- runStateT (unCGIT f) s
         let hs = cgiResponseHeaders s'
         return $ case outp of
           CGIOutput str   -> formatResponse str hs'
               where hs' = tableAddIfNotPresent "Content-type" defaultContentType hs
           CGIRedirect url -> formatResponse FPS.empty hs'
               where hs' = tableSet "Location" url hs

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

formatResponse :: FastString -> [(String,String)]-> FastString
formatResponse c hs = FPS.unlines (map showHeaderFPS hs ++ [FPS.empty, c])
  where showHeaderFPS h = FPS.pack (showHeader h "")

--
-- * Logging and error handling
--

-- | Handle an exception.
--   FIXME: could this be generalized?
handleExceptionCGI :: CGI a -> (Exception -> CGI a) -> CGI a
handleExceptionCGI (CGIT c) h = 
    CGIT (StateT (\s -> f s (runStateT c s))) >>= either h return
  where 
  f s = liftM (either (\ex -> (Left ex,s)) (\(a,s') -> (Right a,s'))) . try

-- | Log some message using the server\'s logging facility.
-- FIXME: does this have to be more general to support
-- FastCGI etc? Maybe we should store log messages in the
-- CGIState?
logCGI :: (MonadCGI m, MonadIO m) => String -> m ()
logCGI s = liftIO (hPutStrLn stderr s)

--
-- * Output \/ redirect
--

-- | Output a 'String'. The output is assumed to be text\/html, encoded using
--   ISO-8859-1. To change this, set the Content-type header using
--   'setHeader'.
output :: MonadCGI m =>
          String        -- ^ The string to output.
       -> m CGIResult
output = return . CGIOutput . FPS.pack

-- | Output a 'FastString'. The output is assumed to be text\/html, 
--   encoded using ISO-8859-1. To change this, set the 
--   Content-type header using 'setHeader'.
outputFPS :: MonadCGI m =>
             FastString        -- ^ The string to output.
          -> m CGIResult
outputFPS = return . CGIOutput

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
getVars = liftM Map.toList $ cgiGet cgiVars

--
-- * Query input
--

-- | Get the value of an input as a 'String'.
inputValue :: Input -> String
inputValue = FPS.unpack . value

-- | Get the value of an input variable, for example from a form.
--   If the variable has multiple values, the first one is returned.
--   Example:
--
-- > query <- getInput "query"
getInput :: MonadCGI m =>
            String           -- ^ The name of the variable.
         -> m (Maybe String) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInput n = lift2M inputValue (getInput_ n)

-- | Like 'getInput', but returns a 'FastString'.
getInputFPS :: MonadCGI m =>
            String           -- ^ The name of the variable.
         -> m (Maybe FastString) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInputFPS n = lift2M value (getInput_ n)

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
getMultiInput n = 
    (map inputValue . Map.findWithDefault [] n) `liftM` cgiGet cgiInput

-- | Get the file name of an input.
getInputFilename :: MonadCGI m =>
                    String           -- ^ The name of the variable.
                 -> m (Maybe String) -- ^ The file name corresponding to the
                                     -- input, if there is one.
getInputFilename n = inside filename (getInput_ n)

getInput_ ::  MonadCGI m => String -> m (Maybe Input)
getInput_ n = 
    (maybe Nothing listToMaybe . Map.lookup n) `liftM` cgiGet cgiInput

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: (Read a, MonadCGI m) =>
             String        -- ^ The name of the variable.
          -> m (Maybe a) -- ^ 'Nothing' if the variable does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readInput name = maybeRead `inside` getInput name

-- | Get the names and values of all inputs.
--   Note: the same name may occur more than once in the output,
--   if there are several values for the name.
getInputs :: MonadCGI m => m [(String,String)]
getInputs = (f . Map.toList) `liftM` cgiGet cgiInput
  where f ps = [ (n,inputValue i) | (n,is) <- ps, i <- is ]

-- | Get the names of all input variables.
getInputNames :: MonadCGI m => m [String]
getInputNames = Map.keys `liftM` cgiGet cgiInput

--
-- * Cookies
--

-- | Get the value of a cookie.
getCookie :: MonadCGI m =>
             String           -- ^ The name of the cookie.
          -> m (Maybe String) -- ^ 'Nothing' if the cookie does not exist.
getCookie name = findCookie name `inside` getVar "HTTP_COOKIE"

-- | Same as 'getCookie', but tries to read the value to the desired type.
readCookie :: (Read a, MonadCGI m) =>
              String       -- ^ The name of the cookie.
            -> m (Maybe a) -- ^ 'Nothing' if the cookie does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readCookie name = maybeRead `inside` getCookie name

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
setHeader n v = modifyHeaders (tableSet n v)

showHeader :: (String,String) -> ShowS
showHeader (n,v) = showString n . showString ": " . showString v

initHeaders :: [(String,String)]
initHeaders = []

--
-- * Utilities
--

mkMultiMap :: Ord a => [(a,b)] -> Map a [b]
mkMultiMap xs = Map.fromListWith (++) [(x,[y]) | (x,y) <- xs]

-- | Replace all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

-- | Set a value in a lookup table with case-insensitive 
--   key comparison.
tableSet :: String -> b -> [(String,b)] -> [(String,b)]
tableSet k v [] = [(k,v)]
tableSet k v ((k',v'):ts)
    | map toLower k == map toLower k' = (k,v) : ts
    | otherwise = (k',v') : tableSet k v ts

-- | Add a key, value pair to a table only if there is no entry
--   with the given key already in the table. If there is an entry
--   already, nothing is done. Case-insensitive key comparison.
tableAddIfNotPresent :: String -> b -> [(String,b)] -> [(String,b)]
tableAddIfNotPresent k v [] = [(k,v)]
tableAddIfNotPresent k v ((k',v'):ts)
    | map toLower k == map toLower k' = (k',v') : ts
    | otherwise = (k',v') : tableAddIfNotPresent k v ts

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

inside :: (Monad m, Monad n) => (a -> n b) -> m (n a) -> m (n b)
inside = liftM . (=<<)

lift2M :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
lift2M = liftM . liftM 

-- | Get the value of an environment variable, or
--   the empty string of the variable is not set.
getEnvOrNil :: String -> IO String
getEnvOrNil v = getEnv v `Prelude.catch` const (return "")

-- | Same as 'lookup' specialized to strings, but 
--   returns the empty string if lookup fails.
lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n



--
-- * Environment variables
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
   , "REQUEST_URI"
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

--
-- * Inputs
--

-- | Get and decode the input according to the request
--   method and the content-type.
decodeInput :: [(String,String)] -- ^ CGI environment variables.
            -> FastString        -- ^ Request body.
            -> [(String,Input)]  -- ^ Input variables and values.
decodeInput env inp = queryInput env ++ bodyInput env inp

-- | Build an 'Input' object for a simple value.
simpleInput :: String -> Input
simpleInput v = Input { value = FPS.pack v,
                        filename = Nothing,
                        contentType = defaultInputType }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?

--
-- * Query string and x-www-form-urlencoded stuff
--

-- | Get inputs from the query string.
queryInput :: [(String,String)] -- ^ CGI environment variables.
           -> [(String,Input)] -- ^ Input variables and values.
queryInput env = formInput $ lookupOrNil "QUERY_STRING" env

-- | Decode application\/x-www-form-urlencoded inputs.
formInput :: String
          -> [(String,Input)] -- ^ Input variables and values.
formInput qs = [(n, simpleInput v) | (n,v) <- formDecode qs]

-- | Get the name-value pairs from application\/x-www-form-urlencoded data.
formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
    where (nv,rs) = break (=='&') s
          (n,v) = break (=='=') nv

-- | Convert a single value from the application\/x-www-form-urlencoded encoding.
urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '



--
-- * Request content and form-data stuff
--

-- | Get input variables from the body, if any.
bodyInput :: [(String,String)] -- ^ CGI environment variables.
          -> FastString        -- ^ Request body.
          -> [(String,Input)]  -- ^ Input variables and values.
bodyInput env inp =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> 
          let ctype = lookup "CONTENT_TYPE" env >>= parseContentType
           in decodeBody ctype $ takeInput env inp
      _ -> []

-- | Decode a POST body.
decodeBody :: Maybe ContentType -- ^ Content-type, if any
           -> FastString        -- ^ Request body
           -> [(String,Input)]  -- ^ Input variables and values.
decodeBody ctype inp = 
    case ctype of
               Just (ContentType "application" "x-www-form-urlencoded" _) 
                   -> formInput (FPS.unpack inp)
               Just (ContentType "multipart" "form-data" ps) 
                   -> multipartDecode ps inp
               Just _ -> [] -- FIXME: report that we don't handle this content type
               -- No content-type given, assume x-www-form-urlencoded
               Nothing -> formInput (FPS.unpack inp)

-- | Take the right number of bytes from the input.
takeInput :: [(String,String)]  -- ^ CGI environment variables.
          -> FastString             -- ^ Request body.
          -> FastString             -- ^ CONTENT_LENGTH bytes from the request body,
                                --   or the empty string if there is no
                                --   CONTENT_LENGTH.
takeInput env req = 
    case len of
           Just l  -> FPS.take l req
           Nothing -> FPS.empty
     where len = lookup "CONTENT_LENGTH" env >>= maybeRead

-- | Decode multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> FastString        -- ^ Request body
                -> [(String,Input)]  -- ^ Input variables and values.
multipartDecode ps inp =
    case lookup "boundary" ps of
         Just b -> case parseMultipartBody b inp of
                        Just (MultiPart bs) -> map bodyPartToInput bs
                        Nothing -> [] -- FIXME: report parse error
         Nothing -> [] -- FIXME: report that there was no boundary

bodyPartToInput :: BodyPart -> (String,Input)
bodyPartToInput (BodyPart hs b) = 
    case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) -> 
                  (lookupOrNil "name" ps,
                   Input { value = b,
                           filename = lookup "filename" ps,
                           contentType = ctype })
              _ -> ("ERROR",simpleInput "ERROR") -- FIXME: report error
    where ctype = fromMaybe defaultInputType (getContentType hs)




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
          input <- FPS.hGetContents stdin
          let str = getRequestInput env input
          h <- connectTo host portId
                 `Exception.catch`
                   (\ e -> abort "Cannot connect to CGI daemon." e)
          FPS.hPut h str >> hPutStrLn h ""
          (sendBack h `finally` hClose h)
               `Prelude.catch` (\e -> unless (isEOFError e) (ioError e))

-- | Returns the query string, or the request body if it is
--   a POST request, or the empty string if there is an error.
getRequestInput :: [(String,String)] -- ^ CGI environment variables.
                -> FastString            -- ^ Request body.
                -> FastString            -- ^ Query string.
getRequestInput env req =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> takeInput env req
      _ -> FPS.pack $ lookupOrNil "QUERY_STRING" env

abort :: String -> Exception -> IO a
abort msg e =
    do putStrLn ("Content-type: text/html\n\n" ++
                   "<html><body>" ++ msg ++ "</body></html>")
       throw e

sendBack :: Handle -> IO ()
sendBack h = do s <- hGetLine h
                putStrLn s
                sendBack h
