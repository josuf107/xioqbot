module Persist where

import Command
import Queue hiding (getQueue)

import qualified Data.Map as Map
import Data.List
import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
import GHC.Generics
import System.Directory

snapshotQueue :: Queue -> IO ()
snapshotQueue q = do
    millisFileName <- fmap (show . floor . (*1000) . utcTimeToPOSIXSeconds) getCurrentTime
    BS.writeFile ("data/" ++ millisFileName) (encodeQueue q)

loadMostRecentQueue :: IO (Either String Queue)
loadMostRecentQueue = do
    queueFiles <- fmap (filter (`notElem` [".", ".."])) (getDirectoryContents "data")
    case reverse (sort queueFiles) of
        (queueFile:_) -> fmap decodeQueue (BS.readFile $ "data/" ++ queueFile)
        [] -> return $ Left "No snapshots"

encodeQueue :: Queue -> BS.ByteString
encodeQueue = encode . SerialQueue

decodeQueue :: BS.ByteString -> Either String Queue
decodeQueue = fmap getSerialQueue . decode

newtype SerialQueue = SerialQueue { getSerialQueue :: Queue } deriving (Show, Eq, Ord)

instance Serialize SerialQueue where
    put (SerialQueue q) = putQueue q
    get = fmap SerialQueue getQueue

putQueue :: Queue -> Put
putQueue q = do
    put (genericizeIndex . queueIndex $ q)

getQueue :: Get Queue
getQueue = do
    index <- fmap ungenericizeIndex get
    return $ defaultQueue
        { queueIndex = index
        }

genericizeIndex
    :: Map.Map TwitchUser (NNID, MiiName, Int, Int)
    -> Map.Map String (String, String, Int, Int)
genericizeIndex
    = Map.mapKeys getTwitchUser
    . fmap (\(NNID nnid, MiiName mii, w, l) -> (nnid, mii, w, l))

ungenericizeIndex
    :: Map.Map String (String, String, Int, Int)
    -> Map.Map TwitchUser (NNID, MiiName, Int, Int)
ungenericizeIndex
    = Map.mapKeys TwitchUser
    . fmap (\(nnid, mii, w, l) -> (NNID nnid, MiiName mii, w, l))

