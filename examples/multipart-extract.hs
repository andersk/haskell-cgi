-- | This program takes a boundary string and a file name as
--   the command line arguments and parses the 
--   contents of the file as multipart\/form-data. The bodies of
--   all the message parts are printed to standard output.
module Main where

import Network.Multipart

import Data.ByteString.Lazy as BS

import System.IO
import System.Environment

readMultipart :: String -> Handle -> IO MultiPart
readMultipart b h = do inp <- BS.hGetContents h
                       case parseMultipartBody b inp of
                         Just x -> return x
                         Nothing -> fail "Couldn't parse input."

savePart :: BodyPart -> IO ()
savePart (BodyPart hs c) = BS.putStr c

main :: IO ()
main = do
       args <- getArgs
       case args of 
                 [b] -> do
                        MultiPart bs <- readMultipart b stdin
                        mapM_ savePart bs
                 _ -> fail "Usage: multipart-extract <boundary> < input"
