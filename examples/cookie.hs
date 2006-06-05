import Network.NewCGI

setCounterCookie :: Int -> CGI ()
setCounterCookie n = setCookie (newCookie "mycookie" (show n))

cgiMain = do mc <- getCookie "mycookie"
             case mc of
               Nothing -> do setCounterCookie 1
                             output "Welcome!"
               Just c  -> do setCounterCookie (read c + 1)
                             output $ "Welcome back! You have been here "
                                       ++ c ++ " times before."

main = runCGI cgiMain