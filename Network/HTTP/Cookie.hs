-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Cookie
-- Copyright   :  (c) Bjorn Bringert, 2004          
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  Bjorn Bringert <bjorn@bringert.net>
-- Stability   :  experimental
-- Portability :  non-portable (uses Network.URI)
--
--  General server side HTTP cookie library.
--  Based on <http://wp.netscape.com/newsref/std/cookie_spec.html>
--
-- TODO
--
-- Add client side stuff (basically parsing Set-Cookie: value)
--
-----------------------------------------------------------------------------
module Network.HTTP.Cookie (
			    Headers, Cookie(..)
			    , newCookie
			    , getCookie, setCookie
			    , findCookie, deleteCookie
			    , showCookie, readCookies
			   ) where

import Data.Char (toLower,isSpace)
import Data.List (intersperse,find)
import Data.Maybe (catMaybes)
import Network.URI (escapeString, unEscapeString, reserved)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Time (CalendarTime(..), Month(..), Day(..),
		    formatCalendarTime)
import Text.ParserCombinators.Parsec

--
-- * Types
--

type Headers = [(String,String)]

-- | Contains all information about a cookie set by the server.
data Cookie = Cookie {
		      -- | Name of the cookie
		      cookieName :: String,
		      -- | Value of the cookie
		      cookieValue :: String,
		      -- | Expiry date of the cookie. If 'Nothing', the
		      --   cookie expires when the browser sessions ends.
		      --   If the date is in the psat, the client should
		      --   delete cookie the cookie immediately.
		      cookieExpires :: Maybe CalendarTime,
		      -- | The domain suffix to which this cookie will be sent
		      cookieDomain :: Maybe String,
		      -- | The path to which this cookie will be sent
		      cookiePath :: Maybe String,
		      -- | 'True' is this cookie should only be sent using
		      --   secure means.
		      cookieSecure :: Bool
		     }
	    deriving (Show, Read, Eq, Ord)

--
-- * Constructing cookies
--

-- | Construct a cookie with only name and value set.
--   This client will expire when the browser sessions ends,
--   will only be sent to the server and path which set it
--   and may be sent using any means.
newCookie :: String -- ^ Name 
	  -> String -- ^ Value
	  -> Cookie -- ^ Cookie
newCookie name value = Cookie { cookieName = name, 
				cookieValue = value,
				cookieExpires = Nothing,
				cookieDomain = Nothing, 
				cookiePath = Nothing,
				cookieSecure = False 
			      }

--
-- * Getting and setting cookies
--

-- | Get the value of a cookie from HTTP request headers
getCookie :: Headers 
	  -> String       -- ^ Cookie name
	  -> Maybe String -- ^ Cookie value
getCookie hs name = 
    find (equalsIgnoreCase "Cookie" . fst) hs >>= findCookie name . snd

-- | Get the value of a cookie from a semicolon separated list of 
--   name-value pairs such as that in the value of the Cookie: header
--   or the HTTP_COOKIE CGI variable.
findCookie :: String -- ^ Cookie name
	   -> String -- ^ Semicolon separated list of name-value pairs 
	   -> Maybe String  -- ^ Cookie value, if found
findCookie name s = maybeLast [ cv | (cn,cv) <- readCookies s, cn == name ]

-- | Set a cookie in HTTP response headers.
setCookie :: Cookie
	  -> Headers
	  -> Headers
-- The cookie is just added to the end of the headers,
-- since the last instance of a cookie overrides earlier ones.
setCookie c hs = hs ++ [("Set-Cookie", showCookie c)]


-- | Delete a cookie from the client by setting the cookie expiry date 
--   to a date in the past.
deleteCookie :: Cookie  -- ^ Cookie to delete. The only fields that matter
		        --   are 'cookieName', 'cookieDomain' and 'cookiePath'
	     -> Cookie
deleteCookie c = c { cookieExpires = Just epoch }
    where
    epoch = CalendarTime {
			  ctYear = 1970,
			  ctMonth = January,
			  ctDay = 1,
			  ctHour = 0,
			  ctMin = 0,
			  ctSec = 0,
			  ctPicosec = 0,
			  ctWDay = Thursday,
			  ctYDay = 1,
			  ctTZName = "GMT",
			  ctTZ = 0,
			  ctIsDST = False
			 }

--
-- * Reading and showing cookies
--

-- | Show a cookie on the format used as the value of the Set-Cookie header.
showCookie :: Cookie -> String
showCookie c = concat $ intersperse "; " $
	        showPair (cookieName c) (cookieValue c) 
	         : catMaybes [expires, domain, path, secure]
    where expires = fmap (showPair "expires" . dateFmt) (cookieExpires c)
	  domain = fmap (showPair "domain") (cookieDomain c)
	  path = fmap (showPair "path") (cookiePath c)
	  secure = if cookieSecure c then Just "secure" else Nothing
	  dateFmt = formatCalendarTime defaultTimeLocale rfc822DateFormat

-- | Show a name-value pair. URI-escapes the name and value.
showPair :: String -- ^ name
	 -> String -- ^ value
	 -> String
showPair name value = escape name ++ "=" ++ escape value
    where escape s = escapeString s (not . reserved)
	  

-- | Gets all the cookies from a Cookie: header value
readCookies :: String             -- ^ String to parse
	    -> [(String,String)]  -- ^ Cookie name - cookie value pairs
readCookies s = case parse parsePairs "" s of 
					   Left _ -> []
					   Right ps -> ps

-- | Parse a semicolon-separated sequence of URI-escaped name=value pairs.
parsePairs :: Parser [(String,String)]
parsePairs = sepBy parsePair (char ';' >> spaces)

-- | Parse a URI-escaped name=value pair
parsePair :: Parser (String,String)
parsePair = do
	    spaces
	    name <- many (satisfy isAllowedChar)
	    spaces
	    char '='
	    spaces
	    value <- many (satisfy isAllowedChar)
	    return (unEscapeString name, unEscapeString value)

-- | Returns true if the character is allowed unescaped in 
--   (the HTTP representation of) cookie names and values.
isAllowedChar :: Char -> Bool
isAllowedChar c | isSpace c = False
isAllowedChar ';' = False
isAllowedChar '=' = False
isAllowedChar ',' = False
isAllowedChar _ = True


--
-- Utilities
--

-- | Compare two strings from equality, ignoring case.
equalsIgnoreCase :: String -> String -> Bool
equalsIgnoreCase x y = map toLower x == map toLower y

-- | Return 'Nothing' is the list is empty, otherwise return
--   the last element of the list.
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just (last xs)
