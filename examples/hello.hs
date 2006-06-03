import Network.NewCGI

cgiMain = output "Hello World!"

main = runCGI cgiMain