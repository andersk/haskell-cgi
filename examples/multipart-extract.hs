import Network.Multipart

import System.IO
import System.Environment

readMultipart :: String -> Handle -> IO MultiPart
readMultipart b h = do
                    inp <- hGetContents h
                    parseM (p_multipart_body b) "<input data>" inp

savePart :: BodyPart -> IO ()
savePart (BodyPart hs c) = putStr c

main :: IO ()
main = do
       args <- getArgs
       case args of 
                 [b] -> do
                        MultiPart bs <- readMultipart b stdin
                        mapM_ savePart bs
                 _ -> fail "Usage: multipart-extract <boundary> < input"
