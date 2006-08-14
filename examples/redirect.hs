-- Redirect to the URL given by the url parameter.

import Network.CGI

printinput = 
    getInput "url" >>= maybe (output "url parameter not set!") redirect

main = runCGI printinput
