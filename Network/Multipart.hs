-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsing of the multipart format from RFC2046.
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module Network.Multipart 
    (
     -- * Multi-part messages
     MultiPart(..), BodyPart(..), Header
    , parseMultipartBody, hGetMultipartBody
     -- * Headers
    , ContentType(..), ContentTransferEncoding(..), 
    , ContentDisposition(..)
    , parseContentType
    , parseContentTransferEncoding
    , parseContentDisposition
    , getContentType
    , getContentTransferEncoding
    , getContentDisposition
    ) where

import Control.Monad
import Data.Char
import Data.List ((\\))
import Data.Maybe
import System.IO (Handle)
import Text.ParserCombinators.Parsec

import qualified Data.FastPackedString as FPS
import Data.FastPackedString (FastString)

--
-- * Multi-part stuff.
--

data MultiPart = MultiPart [BodyPart]
               deriving (Show, Read, Eq, Ord)

data BodyPart = BodyPart [Header] FastString
                deriving (Show, Read, Eq, Ord)

-- | Read a multi-part message from a 'FastString'.
parseMultipartBody :: String -- ^ Boundary
                   -> FastString -> Maybe MultiPart
parseMultipartBody b s = 
    do
    ps <- splitParts (FPS.pack b) s
    liftM MultiPart $ mapM parseBodyPart ps

-- | Read a multi-part message from a 'Handle'.
--   Fails on parse errors.
hGetMultipartBody :: String -- ^ Boundary
                  -> Handle
                  -> IO MultiPart
hGetMultipartBody b h = 
    do
    s <- FPS.hGetContents h
    case parseMultipartBody b s of
        Nothing -> fail "Error parsing multi-part message"
        Just m  -> return m



parseBodyPart :: FastString -> Maybe BodyPart
parseBodyPart s =
    do
    (hdr,bdy) <- splitAtEmptyLine s
    hs <- parseM p_fields "<input>" (FPS.unpack hdr)
    return $ BodyPart hs bdy

--
-- * Splitting into multipart parts.
--

-- | Split a multipart message into the multipart parts.
splitParts :: FastString -- ^ The boundary, without the initial dashes
           -> FastString 
           -> Maybe [FastString]
splitParts b s = dropPreamble b s >>= spl
  where
  spl x = case splitAtBoundary b x of
            Nothing -> Nothing
            Just (s1,d,s2) | isClose b d -> Just [s1]
                           | otherwise -> spl s2 >>= Just . (s1:)

-- | Drop everything up to and including the first line starting 
--   with the boundary. Returns 'Nothing' if there is no 
--   line starting with a boundary.
dropPreamble :: FastString -- ^ The boundary, without the initial dashes
             -> FastString 
             -> Maybe FastString
dropPreamble b s | isBoundary b s = fmap snd (splitAtCRLF s)
                 | otherwise = dropLine s >>= dropPreamble b

-- | Split a string at the first boundary line.
splitAtBoundary :: FastString -- ^ The boundary, without the initial dashes
                -> FastString -- ^ String to split.
                -> Maybe (FastString,FastString,FastString)
                   -- ^ The part before the boundary, the boundary line,
                   --   and the part after the boundary line. The CRLF
                   --   before and the CRLF (if any) after the boundary line
                   --   are not included in any of the strings returned.
                   --   Returns 'Nothing' if there is no boundary.
splitAtBoundary b s = spl 0
  where
  spl i = case findCRLF (FPS.drop i s) of
              Nothing -> Nothing
              Just (j,l) | isBoundary b s2 -> Just (s1,d,s3)
                         | otherwise -> spl (i+j+l)
                  where 
                  s1 = FPS.take (i+j) s
                  s2 = FPS.drop (i+j+l) s
                  (d,s3) = splitAtCRLF_ s2

-- | Check whether a string starts with two dashes followed by
--   the given boundary string.
isBoundary :: FastString -- ^ The boundary, without the initial dashes
           -> FastString
           -> Bool
isBoundary b s = startsWithDashes s && b `FPS.isPrefixOf` FPS.drop 2 s

-- | Check whether a string for which 'isBoundary' returns true
--   has two dashes after the boudary string.
isClose :: FastString -- ^ The boundary, without the initial dashes
        -> FastString 
        -> Bool
isClose b s = startsWithDashes (FPS.drop (2+FPS.length b) s)

-- | Checks whether a string starts with two dashes.
startsWithDashes :: FastString -> Bool
startsWithDashes s = FPS.pack "--" `FPS.isPrefixOf` s


--
-- * RFC 2046 CRLF
--

-- | Drop everything up to and including the first CRLF.
dropLine :: FastString -> Maybe FastString
dropLine s = fmap snd (splitAtCRLF s)

-- | Split a string at the first empty line. The CRLF (if any) before the
--   empty line is included in the first result. The CRLF after the
--   empty line is not included in the result.
--   'Nothing' is returned if there is no empty line.
splitAtEmptyLine :: FastString -> Maybe (FastString, FastString)
splitAtEmptyLine s | startsWithCRLF s = Just (FPS.empty, dropCRLF s)
                   | otherwise = spl 0
  where
  spl i = case findCRLF (FPS.drop i s) of
              Nothing -> Nothing
              Just (j,l) | startsWithCRLF s2 -> Just (s1, dropCRLF s2)
                         | otherwise -> spl (i+j+l)
                where (s1,s2) = FPS.splitAt (i+j+l) s

-- | Split a string at the first CRLF. The CRLF is not included
--   in any of the returned strings.
splitAtCRLF :: FastString -- ^ String to split.
            -> Maybe (FastString,FastString)
            -- ^  Returns 'Nothing' if there is no CRLF.
splitAtCRLF s = case findCRLF s of
                  Nothing -> Nothing
                  Just (i,l) -> Just (s1, FPS.drop l s2)
                      where (s1,s2) = FPS.splitAt i s

-- | Like 'splitAtCRLF', but if no CRLF is found, the first
--   result is the argument string, and the second result is empty.
splitAtCRLF_ :: FastString -> (FastString,FastString)
splitAtCRLF_ s = fromMaybe (s,FPS.empty) (splitAtCRLF s)

-- | Get the index and length of the first CRLF, if any.
findCRLF :: FastString -- ^ String to split.
         -> Maybe (Int,Int)
findCRLF s = 
    case findCRorLF s of
              Nothing -> Nothing
              Just j | j == FPS.length s - 1 -> Just (j,1)
              Just j -> case (FPS.index s j, FPS.index s (j+1)) of
                           ('\n','\r') -> Just (j,2)
                           ('\r','\n') -> Just (j,2)
                           _           -> Just (j,1)

findCRorLF :: FastString -> Maybe Int
findCRorLF s = FPS.findIndex (\c -> c == '\n' || c == '\r') s

startsWithCRLF :: FastString -> Bool
startsWithCRLF s = not (FPS.null s) && (c == '\n' || c == '\r')
  where c = FPS.index s 0

-- | Drop an initial CRLF, if any. If the string is empty, 
--   nothing is done. If the string does not start with CRLF,
--   the first character is dropped.
dropCRLF :: FastString -> FastString
dropCRLF s | FPS.length s <= 1 = FPS.drop 1 s
           | c0 == '\n' && c1 == '\r' = FPS.drop 2 s
           | c0 == '\r' && c1 == '\n' = FPS.drop 2 s
           | otherwise = FPS.drop 1 s
  where c0 = FPS.index s 0
        c1 = FPS.index s 1


-- 
-- * Parsec parsing utilities
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

{-

--
-- * Old multipart stuff.
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
-}

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
