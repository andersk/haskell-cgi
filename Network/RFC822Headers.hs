-----------------------------------------------------------------------------
-- |
-- Module      :  Network.RFC822Headers
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsing of RFC822-style headers (name, value pairs)
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module Network.RFC822Headers where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

type Header = (String, String)

p_fields :: Parser [Header]
p_fields = many p_field

p_field :: Parser Header
p_field = 
    do name <- many1 headerNameChar
       char ':'
       many ws1
       line <- lineString
       crLf
       extraLines <- many extraFieldLine
       return (map toLower name, concat (line:extraLines))

extraFieldLine :: Parser String
extraFieldLine = 
    do sp <- ws1
       line <- lineString
       crLf
       return (sp:line)

--
-- * Parameters (for Content-type etc.)
--


p_parameter :: Parser (String,String)
p_parameter =
  do lexeme $ char ';'
     p_name <- lexeme $ p_token
     lexeme $ char '='
     -- Workaround for seemingly standardized web browser bug
     -- where nothing is escaped in the filename parameter
     -- of the content-disposition header in multipart/form-data
     let litStr = if p_name == "filename" 
                   then buggyLiteralString
                   else literalString
     p_value <- litStr <|> p_token
     return (map toLower p_name, p_value)


-- 
-- * Content type
--

data ContentType = 
	ContentType String String [(String, String)]
    deriving (Show, Read, Eq, Ord)

p_content_type :: Parser ContentType
p_content_type = 
  do many ws1
     c_type <- p_token
     lexeme $ char '/'
     c_subtype <- lexeme $ p_token
     c_parameters <- many p_parameter
     return $ ContentType (map toLower c_type) (map toLower c_subtype) c_parameters

parseContentType :: String -> Maybe ContentType
parseContentType = parseM p_content_type "Content-type"

getContentType :: [Header] -> Maybe ContentType
getContentType hs = lookup "content-type" hs >>= parseContentType

--
-- * Content transfer encoding
--

data ContentTransferEncoding =
	ContentTransferEncoding String
    deriving (Show, Read, Eq, Ord)

p_content_transfer_encoding :: Parser ContentTransferEncoding
p_content_transfer_encoding =
  do many ws1
     c_cte <- p_token
     return $ ContentTransferEncoding (map toLower c_cte)

parseContentTransferEncoding :: String -> Maybe ContentTransferEncoding
parseContentTransferEncoding = 
    parseM p_content_transfer_encoding "Content-transfer-encoding"

getContentTransferEncoding :: [Header] -> Maybe ContentTransferEncoding
getContentTransferEncoding hs = lookup "content-transfer-encoding" hs 
                    >>= parseM p_content_transfer_encoding 
                            "Content-transfer-encoding"

--
-- * Content disposition
--

data ContentDisposition =
	ContentDisposition String [(String, String)]
    deriving (Show, Read, Eq, Ord)

p_content_disposition :: Parser ContentDisposition
p_content_disposition =
  do many ws1
     c_cd <- p_token
     c_parameters <- many p_parameter
     return $ ContentDisposition (map toLower c_cd) c_parameters

parseContentDisposition :: String -> Maybe ContentDisposition
parseContentDisposition = parseM p_content_disposition "Content-disposition"

getContentDisposition :: [Header] -> Maybe ContentDisposition
getContentDisposition hs = 
    lookup "content-disposition" hs  >>= parseContentDisposition

--
-- * Using parsers
--

parseM :: Monad m => Parser a -> SourceName -> String -> m a
parseM p n inp =
  case parse p n inp of
    Left e -> fail (show e)
    Right x -> return x

-- 
-- * Parsing utilities
--

-- | RFC 822 LWSP-char
ws1 :: Parser Char
ws1 = oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme p = do x <- p; many ws1; return x

-- | RFC 822 CRLF (but more permissive)
crLf :: Parser String
crLf = try (string "\n\r" <|> string "\r\n") <|> string "\n" <|> string "\r"

-- | One line
lineString :: Parser String
lineString = many (noneOf "\n\r")

literalString :: Parser String
literalString = do char '\"'
		   str <- many (noneOf "\"\\" <|> quoted_pair)
		   char '\"'
		   return str

-- No web browsers seem to implement RFC 2046 correctly,
-- since they do not escape double quotes and backslashes
-- in the filename parameter in multipart/form-data.
--
-- Note that this eats everything until the last double quote on the line.
buggyLiteralString :: Parser String
buggyLiteralString = 
    do char '\"'
       str <- manyTill anyChar (try lastQuote)
       return str
  where lastQuote = do char '\"' 
                       notFollowedBy (try (many (noneOf "\"") >> char '\"'))

headerNameChar :: Parser Char
headerNameChar = noneOf "\n\r:"

especials, tokenchar :: [Char]
especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials

p_token :: Parser String
p_token = many1 (oneOf tokenchar)

text_chars :: [Char]
text_chars = map chr ([1..9] ++ [11,12] ++ [14..127])

p_text :: Parser Char
p_text = oneOf text_chars

quoted_pair :: Parser Char
quoted_pair = do char '\\'
		 p_text
