-- Accepts file uploads and saves the files in the given directory.
-- WARNING: this script is a SECURITY RISK and only for 
-- demo purposes. Do not put it on a public web server.

import Control.Monad (liftM)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy as BS
import Network.NewCGI

dir = "../upload"

cgiMain = do m <- getInputFilename "file"
             case m of 
               Just n  -> saveFile n
               Nothing -> output form

saveFile n =
    do
    cont <- liftM fromJust $ getInputFPS "file"
    let p = dir ++ "/" ++ basename n
    liftIO $ BS.writeFile p cont
    output $ "Saved as <a href='" ++ p ++ "'>" ++ p ++ "</a>."

form = concat ["<html><body><form method='post' enctype='multipart/form-data'>",
               "<input type='file' name='file' /><br />",
               "<input type='submit' />",
               "</form></body></html>"]

basename = reverse . takeWhile (`notElem` "/\\") . reverse

main = runCGI cgiMain
