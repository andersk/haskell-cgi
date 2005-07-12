#!/usr/bin/env runghc

{-# OPTIONS_GHC -package cgi #-}

-- | Prints the values of all CGI variables and inputs.
module Main where

import Network.NewCGI

printinput :: CGI CGIResult
printinput = 
    do
    setHeader "Content-type" "text/plain"
    vs <- getVars
    is <- getInputs
    output ("Environment:\n" ++ prVars vs
            ++ "\nInputs:\n" ++ prVars is)

prVars vs = unlines [k ++ ": " ++ x | (k,x) <- vs ]

main = runCGI printinput
