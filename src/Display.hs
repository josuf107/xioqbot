{-# LANGUAGE OverloadedStrings #-}
module Display where

import Control.Concurrent.STM
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as LBS

display :: TVar String -> IO ()
display statusLine = do
    run 4444 (myApp statusLine)

myApp :: TVar String -> Request -> (Response -> IO a) -> IO a
myApp statusLine req respond = do
    status <- atomically (readTVar statusLine)
    case pathInfo req of
        [] -> respond (responseLBS status200 [] (LBS.pack html))
        ("status":[]) -> respond (responseLBS status200 [] (LBS.pack status))
        _ -> respond (responseLBS status400 [] (LBS.pack "Not found"))

html :: String
html =
    "<html> \
        \<head> \
            \<style>\
                \p {\
                    \color:white;\
                    \font-size:40px;\
                    \font-family:\"Museo Sans\",sans;\
                \}\
            \</style>\
        \</head>\
        \<body> \
            \<p id=content></p> \
            \<script type=text/javascript> \
                \window.setInterval(updater, 2000); \
                \function updater() { \
                    \var xhttp = new XMLHttpRequest(); \
                    \xhttp.onreadystatechange = function() { \
                      \if (this.readyState == 4 && this.status == 200) { \
                        \document.getElementById(\"content\").innerHTML = this.responseText; \
                      \} \
                    \}; \
                    \xhttp.open(\"GET\", \"status\", true); \
                    \xhttp.send(); \
                \}\
            \</script> \
        \</body> \
    \</html>"
