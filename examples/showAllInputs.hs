import Network.CGI (runCGI, getInputs, ouput)

main =
   runCGI $
      do allInputs <- getInputs
         output $ "Showing all inputs\n"
                ++ (show allInputs)
                ++ "\nDone.\n"

