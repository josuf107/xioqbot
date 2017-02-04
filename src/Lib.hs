module Lib where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Char
import Data.Time
import Network
import System.IO
import Text.Printf
import Text.ParserCombinators.ReadP

import CommandHandler (handleTimestamped, getQueueStatus)
import Persist
import Util
import qualified CommandParser (parseCommand, twitchUser)
import qualified Display
import Queue (Queue, defaultQueue)

data Message
    = Message
    { messageUser :: Maybe String
    , messageCommand :: String
    , messageParams :: [String]
    } deriving (Show,Ord,Eq)

messageP :: ReadP Message
messageP = do
    user <- maybeP (char ':' >> many1 nonSpaceP >>= \user -> char '!' >> return user)
    optional (prefixP >> spaceP)
    command <- commandP
    params <- paramsP
    return (Message user command params)

prefixP, spaceP, commandP, middleP, trailingP :: ReadP String
prefixP = many1 nonSpaceP
spaceP = many1 (char ' ')
commandP = choice [many1 (satisfy isLetter), replicateM 3 numberP]
numberP :: ReadP Char
numberP = satisfy isDigit
paramsP :: ReadP [String]
paramsP = do
    void spaceP
    choice [
        char ':' >> fmap return trailingP,
        middleP >>= \middle -> fmap (middle:) paramsP]
middleP = do
    firstChar <- satisfy (/=':')
    remainder <- many nonSpaceP
    return (firstChar:remainder)
trailingP = manyTill get eof
nonSpaceP :: ReadP Char
nonSpaceP = satisfy (/=' ')

parseMessage :: String -> Message
parseMessage msg = case readP_to_S messageP msg of
    ((cmd, []):_) -> cmd
    x -> error $ "Failed to parse " ++ show x

twitchServer :: HostName
twitchServer = "irc.chat.twitch.tv"

twitchPort :: PortID
twitchPort = PortNumber 6667

connectTwitch :: IO Handle
connectTwitch = connectTo twitchServer twitchPort

loadAndUpdateQueue :: IO Queue
loadAndUpdateQueue = do
    maybeQueueAndLogs <- loadMostRecentQueueAndLogs
    (queueStart, logs) <- case maybeQueueAndLogs of
        Left errorMessage -> putStrLn errorMessage >> return (defaultQueue, [])
        Right (q, logs) -> return (q, logs)
    return $ foldr (\(time, user, msg) -> fst . handleTimestamped user time (CommandParser.parseCommand user msg)) queueStart logs

talk :: String -> String -> String -> IO ()
talk twitchUser twitchToken twitchChannel = do
    putStrLn "Loading saved queue file"
    queue <- loadAndUpdateQueue
    conn <- connectTwitch
    hSetBuffering conn NoBuffering
    write conn "PASS" twitchToken
    write conn "NICK" twitchUser
    write conn "JOIN" twitchChannel
    replicateM_ 10 $ hGetLine conn >>= putStrLn
    write conn "PRIVMSG" (twitchChannel ++ " :Hi everybody")
    statusLine <- atomically (newTVar "")
    displayThread <- forkIO (Display.display statusLine)
    result <- try (handleMessages statusLine twitchChannel conn queue)
    case result of
        Left e -> killThread displayThread >> throw (e :: SomeException)
        Right _ -> return ()
    killThread displayThread

handleMessages :: (TVar String) -> String -> Handle -> Queue -> IO ()
handleMessages statusLine twitchChannel conn q = do
    atomically (writeTVar statusLine (getQueueStatus q))
    nextLine <- hGetLine conn
    q' <- handleMessage twitchChannel conn nextLine q
    handleMessages statusLine twitchChannel conn q'

handleMessage :: String -> Handle -> String -> Queue -> IO Queue
handleMessage twitchChannel conn msg q = do
    time <- getCurrentTime
    putStrLn msg
    case parseMessage msg of
        (Message (Just userString) "PRIVMSG" (_:params)) -> do
            let messageText = reverse . drop 1 . reverse . unwords $ params
            let user = CommandParser.twitchUser userString
            logMessage q time user messageText
            let cmd = CommandParser.parseCommand user messageText
            let (q', maybeReturnMessage) = handleTimestamped user time cmd q
            case maybeReturnMessage of
                Just returnMessage -> write conn "PRIVMSG" (twitchChannel ++ " :" ++ returnMessage)
                Nothing -> return ()
            return q'
        (Message _ "PING" _) -> write conn "PONG" "xioqbot" >> return q
        _ -> return q

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t
