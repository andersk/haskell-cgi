#!/usr/bin/env runghc

{-# OPTIONS_GHC -package cgi #-}

-- | Redirect to the URL given by the url parameter.
module Main where

import Network.NewCGI

printinput :: CGI CGIResult
printinput = 
    do
    murl <- getInput "url"
    case murl of 
	      Nothing  -> output "url parameter not set!"
	      Just url -> redirect url 

main = runCGI printinput
