-- Takes server file path from the file parameter and sends
-- that to the client.
-- WARNING: this script is a SECURITY RISK and only for 
-- demo purposes. Do not put it on a public web server.

import Network.NewCGI

form = concat ["<html><body><form>",
               "<input type='text' name='file' /><br />",
               "<input type='submit' />",
               "</form></body></html>"]

sendFile f = do setHeader "Content-type" "application/octet-stream"
                setHeader "Content-Disposition" ("attachment; filename=" ++ show f)
                outputFile f

cgiMain = getInput "file" >>= maybe (output form) sendFile

main = runCGI cgiMain
