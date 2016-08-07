module Display where

import Data.Time
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

display = do
    run 4444 myApp

myApp req respond = do
    t <- getCurrentTime
    respond (responseLBS status200 [] (LBS.pack (html t)))

html v = "<html><body><p id=content style=color:white;font-size:40px;></p><script type=text/javascript>window.setInterval(myTimer, 1000);function myTimer(){document.getElementById(\"content\").innerHTML=new Date().toLocaleTimeString();}</script></body></html>"
