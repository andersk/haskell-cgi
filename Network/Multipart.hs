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
    , ContentType(..), ContentTransferEncoding(..)
    , ContentDisposition(..)
    , parseContentType
    , parseContentTransferEncoding
    , parseContentDisposition
    , getContentType
    , getContentTransferEncoding
    , getContentDisposition
    ) where

import Control.Monad
import Data.Maybe
import System.IO (Handle)

import Network.RFC822Headers

import qualified Data.ByteString.Char8 as FPS
import Data.ByteString.Char8 (ByteString)

--
-- * Multi-part stuff.
--

data MultiPart = MultiPart [BodyPart]
               deriving (Show, Read, Eq, Ord)

data BodyPart = BodyPart [Header] ByteString
                deriving (Show, Read, Eq, Ord)

-- | Read a multi-part message from a 'ByteString'.
parseMultipartBody :: String -- ^ Boundary
                   -> ByteString -> Maybe MultiPart
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



parseBodyPart :: ByteString -> Maybe BodyPart
parseBodyPart s =
    do
    (hdr,bdy) <- splitAtEmptyLine s
    hs <- parseM p_fields "<input>" (FPS.unpack hdr)
    return $ BodyPart hs bdy

--
-- * Splitting into multipart parts.
--

-- | Split a multipart message into the multipart parts.
splitParts :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString 
           -> Maybe [ByteString]
splitParts b s = dropPreamble b s >>= spl
  where
  spl x = case splitAtBoundary b x of
            Nothing -> Nothing
            Just (s1,d,s2) | isClose b d -> Just [s1]
                           | otherwise -> spl s2 >>= Just . (s1:)

-- | Drop everything up to and including the first line starting 
--   with the boundary. Returns 'Nothing' if there is no 
--   line starting with a boundary.
dropPreamble :: ByteString -- ^ The boundary, without the initial dashes
             -> ByteString 
             -> Maybe ByteString
dropPreamble b s | isBoundary b s = fmap snd (splitAtCRLF s)
                 | otherwise = dropLine s >>= dropPreamble b

-- | Split a string at the first boundary line.
splitAtBoundary :: ByteString -- ^ The boundary, without the initial dashes
                -> ByteString -- ^ String to split.
                -> Maybe (ByteString,ByteString,ByteString)
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
isBoundary :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString
           -> Bool
isBoundary b s = startsWithDashes s && b `FPS.isPrefixOf` FPS.drop 2 s

-- | Check whether a string for which 'isBoundary' returns true
--   has two dashes after the boudary string.
isClose :: ByteString -- ^ The boundary, without the initial dashes
        -> ByteString 
        -> Bool
isClose b s = startsWithDashes (FPS.drop (2+FPS.length b) s)

-- | Checks whether a string starts with two dashes.
startsWithDashes :: ByteString -> Bool
startsWithDashes s = FPS.pack "--" `FPS.isPrefixOf` s


--
-- * RFC 2046 CRLF
--

-- | Drop everything up to and including the first CRLF.
dropLine :: ByteString -> Maybe ByteString
dropLine s = fmap snd (splitAtCRLF s)

-- | Split a string at the first empty line. The CRLF (if any) before the
--   empty line is included in the first result. The CRLF after the
--   empty line is not included in the result.
--   'Nothing' is returned if there is no empty line.
splitAtEmptyLine :: ByteString -> Maybe (ByteString, ByteString)
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
splitAtCRLF :: ByteString -- ^ String to split.
            -> Maybe (ByteString,ByteString)
            -- ^  Returns 'Nothing' if there is no CRLF.
splitAtCRLF s = case findCRLF s of
                  Nothing -> Nothing
                  Just (i,l) -> Just (s1, FPS.drop l s2)
                      where (s1,s2) = FPS.splitAt i s

-- | Like 'splitAtCRLF', but if no CRLF is found, the first
--   result is the argument string, and the second result is empty.
splitAtCRLF_ :: ByteString -> (ByteString,ByteString)
splitAtCRLF_ s = fromMaybe (s,FPS.empty) (splitAtCRLF s)

-- | Get the index and length of the first CRLF, if any.
findCRLF :: ByteString -- ^ String to split.
         -> Maybe (Int,Int)
findCRLF s = 
    case findCRorLF s of
              Nothing -> Nothing
              Just j | j == FPS.length s - 1 -> Just (j,1)
              Just j -> case (FPS.index s j, FPS.index s (j+1)) of
                           ('\n','\r') -> Just (j,2)
                           ('\r','\n') -> Just (j,2)
                           _           -> Just (j,1)

findCRorLF :: ByteString -> Maybe Int
findCRorLF s = FPS.findIndex (\c -> c == '\n' || c == '\r') s

startsWithCRLF :: ByteString -> Bool
startsWithCRLF s = not (FPS.null s) && (c == '\n' || c == '\r')
  where c = FPS.index s 0

-- | Drop an initial CRLF, if any. If the string is empty, 
--   nothing is done. If the string does not start with CRLF,
--   the first character is dropped.
dropCRLF :: ByteString -> ByteString
dropCRLF s | FPS.length s <= 1 = FPS.drop 1 s
           | c0 == '\n' && c1 == '\r' = FPS.drop 2 s
           | c0 == '\r' && c1 == '\n' = FPS.drop 2 s
           | otherwise = FPS.drop 1 s
  where c0 = FPS.index s 0
        c1 = FPS.index s 1
