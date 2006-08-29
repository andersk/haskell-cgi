#!/bin/sh

sed -i.orig -e '/Network\.CGI,/ d' -e 's/Build-depends:\(.*\)/Build-depends: \1, fps/' cgi.cabal
sed -e 's/^module Network\.CGI/module Network.NewCGI/' < Network/CGI.hs > Network/NewCGI.hs
