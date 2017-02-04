module Persist where

import Command
import Queue hiding (getQueue)

import Control.Monad.State (execState)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type LogMessage = (UTCTime, TwitchUser, String)

getMillisFileName :: IO String
getMillisFileName = fmap
    (show . (floor :: RealFrac a => a -> Integer) . (*1000) . utcTimeToPOSIXSeconds)
    getCurrentTime

logMessage :: Queue -> UTCTime -> TwitchUser -> String -> IO ()
logMessage q time user messageText = do
    maybeLogFile <- mostRecentLogFile
    (logFile, logs) <- case maybeLogFile of
        Nothing -> do
            newFileName <- getMillisFileName
            snapshotQueue newFileName q
            return (newFileName, [])
        (Just logFile) -> do
            logs <- loadLogs logFile
            return (logFile, logs)
    currentLogFile <- case length logs >= 100 of
        True -> do
            newFileName <- getMillisFileName
            snapshotQueue newFileName q
            return newFileName
        False -> return logFile
    writeLog currentLogFile time user messageText

mostRecentLogFile :: IO (Maybe FilePath)
mostRecentLogFile = do
    logFiles <- fmap (filter (`notElem` [".", ".."])) (getDirectoryContents "logs")
    return . listToMaybe . reverse . sort $ logFiles

loadLogs :: FilePath -> IO [LogMessage]
loadLogs logFile = do
    logHandle <- openFile ("logs/" ++ logFile) ReadMode
    messages <- fmap (fmap read . lines) . hGetContents $ logHandle
    return messages

writeLog :: FilePath -> UTCTime -> TwitchUser -> String -> IO ()
writeLog fp time user messageText = do
    logHandle <- openFile ("logs/" ++ fp) AppendMode
    hPutStrLn logHandle (show (time, user, messageText))
    hClose logHandle

snapshotQueue :: FilePath -> Queue -> IO ()
snapshotQueue fp q = do
    BS.writeFile ("data/" ++ fp) (encodeQueue q)

loadMostRecentQueue :: IO (Either String Queue)
loadMostRecentQueue = do
    maybeQueueFile <- mostRecentQueueFile
    case maybeQueueFile of
        (Just queueFile) -> fmap decodeQueue (BS.readFile queueFile)
        Nothing -> return $ Left "No snapshots"

mostRecentQueueFile :: IO (Maybe FilePath)
mostRecentQueueFile = do
    queueFiles <- fmap (filter (`notElem` [".", ".."])) (getDirectoryContents "data")
    return . fmap ("data/"++) . listToMaybe . reverse . sort $ queueFiles

loadMostRecentQueueAndLogs :: IO (Either String (Queue, [LogMessage]))
loadMostRecentQueueAndLogs = do
    mostRecentQueue <- loadMostRecentQueue
    recentLogFile <- mostRecentLogFile
    case (mostRecentQueue, recentLogFile) of
        (Right q, Just logFile) -> do
            recentLogs <- loadLogs logFile
            return (Right (q, recentLogs))
        (Left queueError, _) -> return (Left queueError)
        (_, Nothing) -> return (Left "No log files")

encodeQueue :: Queue -> BS.ByteString
encodeQueue = encode . SerialQueue

decodeQueue :: BS.ByteString -> Either String Queue
decodeQueue = fmap getSerialQueue . decode

newtype Serial a = Serial { getSerial :: a } deriving (Show, Eq, Ord)

class Serialize' a where
    put' :: Putter a
    get' :: Get a

instance Serialize' a => Serialize (Serial a) where
    put = put' . getSerial
    get = fmap Serial get'

newtype SerialQueue
    = SerialQueue
    { getSerialQueue :: Queue
    } deriving (Show, Eq, Ord)

instance Serialize SerialQueue where
    put (SerialQueue q) = putQueue q
    get = fmap SerialQueue getQueue

putQueue :: Queue -> Put
putQueue q = do
    let putq f = put (f q)
    putq queueOn
    putq (Serial . queueLastMessage)
    putq (Serial . queueMode)
    putq (Set.map Serial . queueAdmins)
    putq (queueRestricted)
    putq (fmap Serial . queueQueue)
    putq queueOpen
    putq queueSoftClose
    putq (Set.map Serial . queueSoftClosedList)
    putq (Map.mapKeys getTwitchUser . fmap getTeamName . queueTeams)
    putq (getTwitchUser . queueStreamer)
    mapM_ putq [queueSetWins, queueSetLosses, queueCrewStockA, queueCrewStockB]
    putq (Set.map getTwitchUser . queueFriendMes)
    put (genericizeIndex . queueIndex $ q)
    putq queueRulesSingles
    putq queueRulesDoubles
    putq (Map.mapKeys Serial . fmap Serial . queueHereMap)
    putq (Map.mapKeys Serial . fmap (Set.map Serial) . queueInvites)
    putq queueDenyReply
    putq queueWinBuffer
    putq queueListBuffer
    putq queueSinglesLimit
    putq queueDoublesLimit
    putq queueReenterWait
    putq queueHereAlert
    putq queueSinglesBestOf
    putq queueDoublesBestOf

getSet :: Serialize a => QueueSet a -> Get (Endo Queue)
getSet f = fmap (Endo . execState . f) get

getModify :: Serialize a => QueueModify a -> Get (Endo Queue)
getModify m = getSet (m . const)

getModifyF :: Serialize a => (a -> b) -> QueueModify b -> Get (Endo Queue)
getModifyF f m = getSet (m . const . f)

getSet' :: Serialize' a => QueueSet a -> Get (Endo Queue)
getSet' f = fmap (Endo . execState . f . getSerial) get

getModify' :: Serialize' a => QueueModify a -> Get (Endo Queue)
getModify' m = getSet' (m . const)

getQueue :: Get Queue
getQueue = do
    parts <- sequence
        [ getSet setQueueOn
        , getSet' setQueueLastMessage
        , getSet' setQueueMode
        , getModify' withQueueAdmins
        , getModify withQueueRestricted
        , getModify' withQueueQueue
        , getSet setQueueOpen
        , getSet setQueueSoftClose
        , getModify' withQueueSoftClosedList
        , getModify' withQueueTeams
        , getSet' setQueueStreamer
        , getModify withQueueSetWins
        , getModify withQueueSetLosses
        , getSet setQueueCrewStockA
        , getSet setQueueCrewStockB
        , getModify' withQueueFriendMes
        , getModifyF ungenericizeIndex withQueueIndex
        , getSet setQueueRulesSingles
        , getSet setQueueRulesDoubles
        , getModify' withQueueHereMap
        , getModify' withQueueInvites
        , getSet setQueueDenyReply
        , getSet setQueueWinBuffer
        , getSet setQueueListBuffer
        , getSet setQueueSinglesLimit
        , getSet setQueueDoublesLimit
        , getSet setQueueReenterWait
        , getSet setQueueHereAlert
        , getSet setQueueSinglesBestOf
        , getSet setQueueDoublesBestOf
        ]
    return $ appEndo (mconcat parts) defaultQueue

instance Serialize' TwitchUser where
    put' = put . getTwitchUser
    get' = fmap TwitchUser get
instance Serialize' TeamName where
    put' = put . getTeamName
    get' = fmap TeamName get
instance Serialize' UserOrTeam where
    put' = put . getUserOrTeam
    get' = fmap UserOrTeam get
instance Serialize' NNID where
    put' = put . getNNID
    get' = fmap NNID get
instance Serialize' MiiName where
    put' = put . getMiiName
    get' = fmap MiiName get
instance Serialize' Mode where
    put' mode = case mode of
        Singles -> putWord8 0
        Doubles -> putWord8 1
        Crew -> putWord8 2
        InvalidMode invalidString -> putWord8 3 >> put invalidString
    get' = getWord8 >>= \w -> case w of
        0 -> return Singles
        1 -> return Doubles
        2 -> return Crew
        3 -> fmap InvalidMode get
        x -> return $ InvalidMode ("No parse: " ++ show x)
instance Serialize' CrewSide where
    put' crew = case crew of
        A -> putWord8 0
        B -> putWord8 1
    get' = getWord8 >>= \w -> return $ case w of
        0 -> A
        1 -> B
        x -> error $ "Couldn't parse " ++ show x
instance Serialize' UTCTime where
    put' (UTCTime (ModifiedJulianDay d) t) = do
        put d
        put (toRational t)
    get' = do
        d <- fmap ModifiedJulianDay get
        t <- fmap fromRational get
        return $ UTCTime d t
instance (Ord a, Serialize' a) => Serialize' (Set.Set a) where
    put' = put . Set.map Serial
    get' = fmap (Set.map getSerial) get
instance Serialize' a => Serialize' (Seq.Seq a) where
    put' = put . fmap Serial
    get' = fmap (fmap getSerial) get
instance (Ord k, Serialize' k, Serialize' v) => Serialize' (Map.Map k v) where
    put' = put . Map.mapKeys Serial . fmap Serial
    get' = fmap (Map.mapKeys getSerial . fmap getSerial) get

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
