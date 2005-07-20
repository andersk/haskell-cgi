-- parsing of the multipart format from RFC2045

-- based on code from WASHMail, © 2001, 2002 Peter Thiemann
module Network.Multipart where

import Data.Char
import Data.List ((\\))
import Text.ParserCombinators.Parsec



-- 
-- * Parsing utilities
--

-- RFC 822 LWSP-char
ws1 = oneOf " \t"

lexeme p = do x <- p; many ws1; return x

-- RFC 822 CRLF (but more permissive)
crLf = try (string "\n\r" <|> string "\r\n") <|> string "\n" <|> string "\r"

lineChar = noneOf "\n\r"

lineString = many lineChar

literalString = do char '\"'
		   str <- many (noneOf "\"\\" <|> quoted_pair)
		   char '\"'
		   return str

headerNameChar = noneOf "\n\r:"

especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials
p_token = many1 (oneOf tokenchar)

text_chars = map chr ([1..9] ++ [11,12] ++ [14..127])
p_text = oneOf text_chars

quoted_pair = do char '\\'
		 p_text

--
-- * Multipart stuff
--

data MultiPart = MultiPart [BodyPart]
               deriving Show

data BodyPart = BodyPart [Header] String
                deriving Show 

-- Based on grammar from RFC 2046, section 5.1.1
-- The delimiters have been moved around
-- in the grammar to be able to decide where
-- body parts end.

p_multipart_body :: String  -- ^ Boundary
                 -> GenParser Char () MultiPart
p_multipart_body boundary =
    do p_start_boundary boundary
       bps <- many (p_encapsulation boundary)
       string "--"
       p_transport_padding
       p_epilogue
       return (MultiPart bps)

p_transport_padding = many ws1

p_encapsulation boundary =
    do p_transport_padding
       crLf
       p_body_part boundary

p_dash_boundary boundary =
    do string "--"
       string boundary
       return ()

-- preamble and first dash boundary
p_start_boundary boundary = 
    try (p_dash_boundary boundary) 
            <|> (do
                 lineString
                 crLf
                 p_start_boundary boundary)

p_epilogue = skipMany anyChar

p_body_part boundary = 
    do hs <- p_fields
       crLf
       try (p_dash_boundary boundary >> return (BodyPart hs []))
               <|> (do l <- lineString
                       cs <- p_body_rest boundary
                       return (BodyPart hs (l++cs)))

p_body_rest boundary =
    do n <- crLf
       try (p_dash_boundary boundary >> return [])
               <|> (do l <- lineString
                       ls <- p_body_rest boundary
                       return (n++l++ls))

-- 
-- * Header
--

type Header = (String, String)

p_fields :: CharParser () [Header]
p_fields = many p_field

p_field :: CharParser () Header
p_field = 
    do name <- many1 headerNameChar
       char ':'
       many ws1
       line <- lineString
       crLf
       extraLines <- many extraFieldLine
       return (map toLower name, concat (line:extraLines))

extraFieldLine = 
    do sp <- ws1
       line <- lineString
       crLf
       return (sp:line)

--
-- * Parameters (for Content-type etc.)
--

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
	ContentType String -- type
		    String -- subtype
		    [(String, String)] -- parameters
    deriving Show

p_content_type :: CharParser () ContentType
p_content_type = 
  do many ws1
     c_type <- p_token
     lexeme $ char '/'
     c_subtype <- lexeme $ p_token
     c_parameters <- many p_parameter
     return $ ContentType (map toLower c_type) (map toLower c_subtype) c_parameters

--
-- * Content transfer encoding
--

data ContentTransferEncoding =
	ContentTransferEncoding String
    deriving Show

p_content_transfer_encoding =
  do many ws1
     c_cte <- p_token
     return $ ContentTransferEncoding (map toLower c_cte)

--
-- * Content disposition
--

data ContentDisposition =
	ContentDisposition String [(String, String)]
    deriving Show

p_content_disposition =
  do many ws1
     c_cd <- p_token
     c_parameters <- many p_parameter
     return $ ContentDisposition (map toLower c_cd) c_parameters



parseMaybe :: CharParser () a -> SourceName -> String -> Maybe a
parseMaybe p n inp =
  case parse p n inp of
    Left pError -> Nothing
    Right x -> Just x

parseSuccessfully :: CharParser () a -> SourceName -> String -> a
parseSuccessfully p n inp =
  case parse p n inp of
    Left pError ->
      error (show pError)
    Right x ->
      x
