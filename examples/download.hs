#!/usr/bin/env runghc

{-# OPTIONS_GHC -package cgi #-}

-- | Takes server file path from the file parameter and sends
--   that to the client.
--   WARNING: this script is a SECURITY RISK and only for 
--   demo purposes. Do not put it on a public web server.
module Main where

import Network.NewCGI

download =
    do m <- getInput "file"
       case m of 
                 Just n  -> do                            
                            setHeader "Content-type" "application/octet-stream"
                            outputFile n
                 Nothing -> printForm

printForm =
    output $ "<html><body><form>"
               ++ "<input type='text' name='file' /><br />"
               ++ "<input type='submit' />"
               ++ "</form></body></html>"

main = runCGI download
