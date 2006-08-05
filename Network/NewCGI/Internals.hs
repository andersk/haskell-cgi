{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.NewCGI.Internals
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal stuff that most people shouldn't have to use.
-- This module mostly deals which the CGI protocol side,
-- and the internals of the CGIT monad transformer.
-- This can for example be used to write alternative run functions.
--
-----------------------------------------------------------------------------

module Network.NewCGI.Internals (
    MonadCGI(..), CGIRequest(..), CGIT(..), CGIResult(..), CGI
  , Input(..), Headers, HeaderName(..)
  , hRunCGI, runCGIEnv, runCGIEnvFPS
  -- * Error handling
  , throwCGI, catchCGI, tryCGI, handleExceptionCGI
  -- * Logging
  , logCGI
  -- * Environment variables
  , getCGIVars
  -- * Inputs
  , takeInput
  -- * URL encoding
  , formEncode, urlEncode, formDecode, urlDecode
  -- * Utilities
  , maybeRead
 ) where

import Control.Exception as Exception (Exception, try, throwIO)
import Control.Monad (liftM)
import Control.Monad.Error (MonadError(..))
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Data.Char (toLower)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Monoid (mempty)
import Network.URI (unEscapeString,escapeURIString,isUnescapedInURI)
import System.Environment (getEnvironment)
import System.IO (Handle, hPutStrLn, stderr, hFlush)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

import Network.Multipart

--
-- * CGI requests
--

-- | The input to a CGI action.
data CGIRequest = 
    CGIRequest {
                -- | Environment variables.
                cgiVars :: Map String String,
                -- | Input parameters. For better laziness in reading inputs,
                --   this is not a Map.
                cgiInputs :: [(String, Input)],
                -- | Raw request body. 
                cgiRequestBody :: ByteString
               }
    deriving Show

-- | The value of an input parameter, and some metadata.
data Input = Input {
                    value :: ByteString,
                    filename :: Maybe String,
                    contentType :: ContentType
                   }
              deriving Show

--
-- * CGI results
--

-- | The result of a CGI program.
data CGIResult = CGIOutput ByteString
               | CGINothing
                 deriving (Show, Read, Eq, Ord)

--
-- * Response headers
--

type Headers = [(HeaderName, String)]

-- | A string with case insensitive equality and comparisons.
newtype HeaderName = HeaderName String deriving (Show)

instance Eq HeaderName where
    HeaderName x == HeaderName y = map toLower x == map toLower y

instance Ord HeaderName where
    HeaderName x `compare` HeaderName y = map toLower x `compare` map toLower y


--
-- * CGIT monad transformer
--

-- | A simple CGI monad with just IO.
type CGI a = CGIT IO a

-- | The CGIT monad transformer.
newtype CGIT m a = CGIT { unCGIT :: ReaderT CGIRequest (WriterT Headers m) a }

instance Monad m => Functor (CGIT m) where
    fmap f c = CGIT (fmap f (unCGIT c))

instance Monad m => Monad (CGIT m) where
    c >>= f = CGIT (unCGIT c >>= unCGIT . f)
    return = CGIT . return
    -- FIXME: should we have an error monad instead?
    fail = CGIT . fail

instance MonadIO m => MonadIO (CGIT m) where
    liftIO = lift . liftIO

-- | The class of CGI monads. Most CGI actions can be run in
--   any monad which is an instance of this class, which means that
--   you can use your own monad transformers to add extra functionality.
class Monad m => MonadCGI m where
    -- | Add a response header.
    cgiAddHeader :: HeaderName -> String -> m ()
    -- | Get something from the CGI request.
    cgiGet :: (CGIRequest -> a) -> m a

instance Monad m => MonadCGI (CGIT m) where
    cgiAddHeader n v = CGIT $ lift $ tell [(n,v)]
    cgiGet = CGIT . asks

instance MonadTrans CGIT where
    lift = CGIT . lift . lift

-- requires -fallow-undecidable-instances and -fallow-overlapping-instances
instance (MonadTrans t, MonadCGI m, Monad (t m)) => MonadCGI (t m) where
    cgiAddHeader n v = lift $ cgiAddHeader n v
    cgiGet = lift . cgiGet


runCGIT :: CGIT m a -> CGIRequest -> m (a, Headers)
runCGIT (CGIT c) r = runWriterT $ runReaderT c r

--
-- * Running CGI actions
--

-- | Run a CGI action. Gets CGI environment variables from
--   the program environment.
hRunCGI :: MonadIO m =>
           Handle -- ^ Handle that input will be read from.
        -> Handle -- ^ Handle that output will be written to.
        -> CGIT m CGIResult -> m ()
hRunCGI hin hout f = do env <- liftIO getCGIVars
                        inp <- liftIO $ BS.hGetContents hin
                        outp <- runCGIEnvFPS env inp f
                        liftIO $ BS.hPut hout outp
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
runCGIEnv vars inp f = liftM BS.unpack $ runCGIEnvFPS vars (BS.pack inp) f

-- | Run a CGI action in a given environment, using a 'FastString'
--   for input and a lazy string for output. 
runCGIEnvFPS :: Monad m =>
             [(String,String)] -- ^ CGI environment variables.
          -> ByteString -- ^ Request body.
          -> CGIT m CGIResult -- ^ CGI action.
          -> m ByteString -- ^ Response (headers and content).
runCGIEnvFPS vars inp f
    = do let s = CGIRequest {
                             cgiVars = Map.fromList vars,
                             cgiInputs = decodeInput vars inp,
                             cgiRequestBody = inp
                            }
         (outp,hs) <- runCGIT f s
         return $ case outp of
           CGIOutput c -> formatResponse c hs'
               where hs' = if isJust (lookup ct hs)
                              then hs else hs ++ [(ct,defaultContentType)]
                     ct = HeaderName "Content-type"
           CGINothing -> formatResponse BS.empty hs

formatResponse :: ByteString -> Headers -> ByteString
formatResponse c hs = 
    BS.unlines ([BS.pack (n++": "++v) | (HeaderName n,v) <- hs] 
                ++ [BS.empty,c])

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

--
-- * Error handling
--

instance MonadError Exception (CGIT IO) where
    throwError = throwCGI
    catchError = catchCGI

-- | Throw an exception in a CGI monad. The monad is required to be
--   a 'MonadIO', so that we can use 'throwIO' to guarantee ordering.
throwCGI :: (MonadCGI m, MonadIO m) => Exception -> m a
throwCGI = liftIO . throwIO

-- | Catches any expection thrown by a CGI action, and uses the given 
--   exception handler if an exception is thrown.
catchCGI :: CGI a -> (Exception -> CGI a) -> CGI a
catchCGI c h = tryCGI c >>= either h return

-- | Catches any exception thrown by an CGI action, and returns either
--   the exception, or if no exception was raised, the result of the action.
tryCGI :: CGI a -> CGI (Either Exception a)
tryCGI (CGIT c) = CGIT (ReaderT (\r -> WriterT (f (runWriterT (runReaderT c r)))))
    where
      f = liftM (either (\ex -> (Left ex,mempty)) (\(a,w) -> (Right a,w))) . try

{-# DEPRECATED handleExceptionCGI "Use catchCGI instead." #-}
-- | Deprecated version of 'catchCGI'. Use 'catchCGI' instead.
handleExceptionCGI :: CGI a -> (Exception -> CGI a) -> CGI a
handleExceptionCGI = catchCGI

--
-- * Logging
--

-- | Log some message using the server\'s logging facility.
-- FIXME: does this have to be more general to support
-- FastCGI etc? Maybe we should store log messages in the
-- CGIState?
logCGI :: (MonadCGI m, MonadIO m) => String -> m ()
logCGI s = liftIO (hPutStrLn stderr s)

--
-- * Environment variables
--

getCGIVars :: IO [(String,String)]
getCGIVars = getEnvironment

--
-- * Inputs
--

-- | Get and decode the input according to the request
--   method and the content-type.
decodeInput :: [(String,String)] -- ^ CGI environment variables.
            -> ByteString        -- ^ Request body.
            -> [(String,Input)]  -- ^ Input variables and values.
decodeInput env inp = queryInput env ++ bodyInput env inp

-- | Build an 'Input' object for a simple value.
simpleInput :: String -> Input
simpleInput v = Input { value = BS.pack v,
                        filename = Nothing,
                        contentType = defaultInputType }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?


--
-- * Query string
--

-- | Get inputs from the query string.
queryInput :: [(String,String)] -- ^ CGI environment variables.
           -> [(String,Input)] -- ^ Input variables and values.
queryInput env = formInput $ lookupOrNil "QUERY_STRING" env

-- | Decode application\/x-www-form-urlencoded inputs.
formInput :: String
          -> [(String,Input)] -- ^ Input variables and values.
formInput qs = [(n, simpleInput v) | (n,v) <- formDecode qs]

--
-- * URL encoding
--

-- | Format name-value pairs as application\/x-www-form-urlencoded.
formEncode :: [(String,String)] -> String
formEncode xs = 
    concat $ intersperse "&" [urlEncode n ++ "=" ++ urlEncode v | (n,v) <- xs]

-- | Convert a single value to the application\/x-www-form-urlencoded encoding.
urlEncode :: String -> String
urlEncode = replace ' ' '+' . escapeURIString okChar
  where okChar c = c == ' ' || 
                   (isUnescapedInURI c && c `notElem` "&=+")

-- | Get the name-value pairs from application\/x-www-form-urlencoded data.
formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
    where (nv,rs) = break (=='&') s
          (n,v) = break (=='=') nv

-- | Convert a single value from the 
--   application\/x-www-form-urlencoded encoding.
urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '

--
-- * Request content and form-data stuff
--

-- | Get input variables from the body, if any.
bodyInput :: [(String,String)] -- ^ CGI environment variables.
          -> ByteString        -- ^ Request body.
          -> [(String,Input)]  -- ^ Input variables and values.
bodyInput env inp =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> 
          let ctype = lookup "CONTENT_TYPE" env >>= parseContentType
           in decodeBody ctype $ takeInput env inp
      _ -> []

-- | Decode a POST body.
decodeBody :: Maybe ContentType -- ^ Content-type, if any
           -> ByteString        -- ^ Request body
           -> [(String,Input)]  -- ^ Input variables and values.
decodeBody ctype inp = 
    case ctype of
               Just (ContentType "application" "x-www-form-urlencoded" _) 
                   -> formInput (BS.unpack inp)
               Just (ContentType "multipart" "form-data" ps) 
                   -> multipartDecode ps inp
               Just _ -> [] -- FIXME: report that we don't handle this content type
               -- No content-type given, assume x-www-form-urlencoded
               Nothing -> formInput (BS.unpack inp)

-- | Take the right number of bytes from the input.
takeInput :: [(String,String)]  -- ^ CGI environment variables.
          -> ByteString         -- ^ Request body.
          -> ByteString         -- ^ CONTENT_LENGTH bytes from the request 
                                --   body, or the empty string if there is no
                                --   CONTENT_LENGTH.
takeInput env req = 
    case len of
           Just l  -> BS.take l req
           Nothing -> BS.empty
     where len = lookup "CONTENT_LENGTH" env >>= maybeRead

-- | Decode multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> ByteString        -- ^ Request body
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
-- * Utilities
--

-- | Replace all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Same as 'lookup' specialized to strings, but 
--   returns the empty string if lookup fails.
lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n

