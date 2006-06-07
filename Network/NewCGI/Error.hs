-----------------------------------------------------------------------------
-- |
-- Module      :  Network.NewCGI.Error
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A simple error handler for NewCGI programs which outputs error pages
-- and logs errors.
--
-----------------------------------------------------------------------------

module Network.NewCGI.Error where

import Control.Exception
import Data.List (intersperse)

import Network.NewCGI hiding (Html)
import Text.XHtml


--
-- * Error handling
--

handleErrors :: CGI CGIResult -> CGI CGIResult
handleErrors x = catchCGI x outputException

--
-- * Error output
--

-- | Output a 500 Internal Server Error with information from
--   an Exception.
outputException :: (MonadCGI m,MonadIO m) => Exception -> m CGIResult
outputException e = 
    case e of
      ErrorCall msg -> err msg
      _             -> err (show e)
  where err msg = outputError 500 "Internal Server Error" [msg]

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
         setHeader "Status" (show c)
         output $ renderHtml $ errorPage c m es 

-- | Create an HTML error page.
errorPage :: Int      -- ^ Status code
          -> String   -- ^ Status message
          -> [String] -- ^ Error information
          -> Html
errorPage c m es = header << thetitle << tit 
                   +++ body << (h1 << tit +++ map (paragraph <<) es 
                                +++ hr +++ paragraph << sig)
    where tit = show c ++ " " ++ m
          sig = "Network.NewCGI/X.X" -- FIXME: get from somewhere

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
                             String -- ^ Error message.
                          -> m CGIResult
outputInternalServerError e = outputError 500 "Internal Server Error" [e]
