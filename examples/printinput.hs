-- Prints the values of all CGI variables and inputs.

import Network.NewCGI

import Data.List (intersperse)

cgiMain :: CGI CGIResult
cgiMain = do vs <- getVars
             is <- getInputNames
             i <- mapM prInput is
             setHeader "Content-type" "text/plain"
             output ("Environment:\n" ++ prVars vs
                     ++ "\nInputs:\n" ++ unlines i)

prInput :: String -> CGI String
prInput i = 
    do
    vs <- getMultiInput i
    let v = concat $ intersperse "," $ map show vs
    f <- getInputFilename i
    return $ case f of
           Just n -> i ++ ": File\nfilename=" ++ n
                     ++ "\ncontents=" ++ v
           Nothing -> i ++ ": " ++ v

prVars vs = unlines [k ++ ": " ++ x | (k,x) <- vs ]

main = runCGI cgiMain
