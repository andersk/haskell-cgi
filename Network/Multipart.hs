-- parsing of the multipart format from RFC2046

-- based on code from WASHMail, © 2001, 2002 Peter Thiemann
module Network.Multipart where

import Data.Char
import Data.List ((\\))
import Text.ParserCombinators.Parsec



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

--
-- * Multipart stuff
--

data MultiPart = MultiPart [BodyPart]
               deriving (Show, Read, Eq, Ord)

data BodyPart = BodyPart [Header] String
                deriving (Show, Read, Eq, Ord)

-- Based on grammar from RFC 2046, section 5.1.1
-- The delimiters have been moved around
-- in the grammar to be able to decide where
-- body parts end.

p_multipart_body :: String  -- ^ Boundary
                 -> Parser MultiPart
p_multipart_body boundary =
    do p_start_boundary boundary
       bps <- many (do
                    p_transport_padding
                    crLf
                    p_body_part boundary)
       string "--"
       p_transport_padding
       skipMany anyChar -- discard epilogue
       return (MultiPart bps)

p_transport_padding :: Parser String
p_transport_padding = many ws1

p_dash_boundary :: String -> Parser ()
p_dash_boundary boundary =
    do string "--"
       string boundary
       return ()

-- | Parse preamble and first dash-boundary
p_start_boundary :: String -> Parser ()
p_start_boundary boundary = 
    try (p_dash_boundary boundary) 
            <|> (do
                 lineString -- discard a preamble line
                 crLf
                 p_start_boundary boundary)

p_body_part :: String -> Parser BodyPart
p_body_part boundary = 
    do hs <- p_fields
       crLf
       try (p_dash_boundary boundary >> return (BodyPart hs []))
               <|> (do l <- lineString
                       cs <- p_body_rest
                       return (BodyPart hs (l++cs)))
    where p_body_rest =
              do n <- crLf
                 try (p_dash_boundary boundary >> return [])
                         <|> (do l <- lineString
                                 ls <- p_body_rest
                                 return (n++l++ls))

-- 
-- * Headers
--

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
     p_value <- literalString <|> p_token
     return (map toLower p_name, p_value)

-- 
-- * Content type
--

data ContentType = 
	ContentType String             -- ^type
		    String             -- ^ subtype
		    [(String, String)] -- ^ parameters
    deriving (Show, Read, Eq, Ord)

p_content_type :: Parser ContentType
p_content_type = 
  do many ws1
     c_type <- p_token
     lexeme $ char '/'
     c_subtype <- lexeme $ p_token
     c_parameters <- many p_parameter
     return $ ContentType (map toLower c_type) (map toLower c_subtype) c_parameters

getContentType :: [Header] -> Maybe ContentType
getContentType hs = lookup "content-type" hs 
                    >>= parseMaybe p_content_type "Content-type"

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

getContentTransferEncoding :: [Header] -> Maybe ContentTransferEncoding
getContentTransferEncoding hs = lookup "content-transfer-encoding" hs 
                    >>= parseMaybe p_content_transfer_encoding 
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

getContentDisposition :: [Header] -> Maybe ContentDisposition
getContentDisposition hs = lookup "content-disposition" hs 
                    >>= parseMaybe p_content_disposition "Content-disposition"

--
-- * Using parsers
--

parseMaybe :: Parser a -> SourceName -> String -> Maybe a
parseMaybe p n inp =
  case parse p n inp of
    Left _ -> Nothing
    Right x -> Just x

parseSuccessfully :: Parser a -> SourceName -> String -> a
parseSuccessfully p n inp =
  case parse p n inp of
    Left pError -> error (show pError)
    Right x -> x
