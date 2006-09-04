{-# OPTIONS_GHC -fallow-overlapping-instances #-}

import Network.NewCGI
import Text.XHtml

import Control.Monad

f n = liftM (((,) n) . show)

page = liftM defList $ sequence 
       [
        f "serverName"           serverName,
        f "serverPort"           (liftM show serverPort),
        f "requestMethod"        requestMethod,
        f "pathInfo"             pathInfo,
        f "pathTranslated"       pathTranslated,
        f "scriptName"           scriptName,
        f "remoteHost"           remoteHost,
        f "remoteAddr"           remoteAddr,
        f "authType"             authType,
        f "remoteUser"           remoteUser,
        f "requestContentType"   requestContentType ,
        f "requestContentLength" requestContentLength,
        f "requestHeader \"User-Agent\"" (requestHeader "User-Agent")
       ]

cgiMain = page >>= output . renderHtml

main = runCGI cgiMain