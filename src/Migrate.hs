module Migrate where

import Queue hiding (getQueue)
import Persist

import Data.Serialize
import qualified Data.ByteString as BS

migrate :: IO ()
migrate = do
    queueFile <- mostRecentQueueFile
    case queueFile of
        Nothing -> putStrLn "No recent queue file"
        (Just queueFile) -> do
            oldQueue <- (fmap oldDecodeQueue . BS.readFile) queueFile
            either (\e -> putStrLn ("Error parsing queue file " ++ e)) saveNewQueue oldQueue

saveNewQueue :: Queue -> IO ()
saveNewQueue queue = do
    newFileName <- getMillisFileName
    snapshotQueue newFileName queue
    putStrLn ("Saved new queue " ++ newFileName)

oldDecodeQueue :: BS.ByteString -> Either String Queue
oldDecodeQueue = fmap getOldSerialQueue . decode

newtype OldSerialQueue
    = OldSerialQueue
    { getOldSerialQueue :: Queue
    } deriving (Show, Eq, Ord)

instance Serialize OldSerialQueue where
    put (OldSerialQueue q) = error "Don't try to serialize the old version of the queue"
    get = fmap OldSerialQueue getOldQueue

getOldQueue :: Get Queue
getOldQueue = do
    index <- fmap ungenericizeIndex get
    return $ defaultQueue
        { queueIndex = index
        }
