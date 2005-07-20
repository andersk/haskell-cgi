#!/usr/bin/env runghc

{-# OPTIONS_GHC -package cgi #-}

-- | Prints the values of all CGI variables and inputs.
module Main where

import Network.NewCGI

import Control.Monad (liftM)
import Data.Maybe (fromJust)

printinput :: CGI CGIResult
printinput =
    do
    setHeader "Content-type" "text/plain"
    vs <- getVars
    is <- getInputNames
    i <- mapM prInput is
    output ("Environment:\n" ++ prVars vs
            ++ "\nInputs:\n" ++ unlines i)

prInput :: String -> CGI String
prInput i = 
    do
    v <- liftM fromJust (getInput i)
    f <- getInputFilename i
    return $ case f of
           Just n -> i ++ ": File\nfilename=" ++ n
                     ++ "\ncontents=" ++ v
           Nothing -> i ++ ": " ++ v 

prVars vs = unlines [k ++ ": " ++ x | (k,x) <- vs ]

main = runCGI printinput
