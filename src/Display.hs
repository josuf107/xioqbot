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
    respond (responseLBS status200 [] (LBS.pack (show t)))
