module Lib where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Time
import Network
import System.IO
import Text.Printf
import Text.ParserCombinators.ReadP

import Command hiding (spaceP, commandP)
import Queue hiding (spaceP, commandP, Message)
import Persist

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
prefixP = many1 nonSpaceP
spaceP = many1 (char ' ')
commandP = choice [many1 (satisfy isLetter), replicateM 3 numberP]
numberP = satisfy isDigit
paramsP = go []
    where
        go params = do
            spaceP
            choice [
                char ':' >> fmap return trailingP,
                middleP >>= \middle -> fmap (middle:) paramsP]
middleP = do
    firstChar <- satisfy (/=':')
    remainder <- many nonSpaceP
    return (firstChar:remainder)
trailingP = manyTill get eof
nonSpaceP = satisfy (/=' ')

parseMessage msg = case readP_to_S messageP msg of
    ((cmd, []):_) -> cmd
    x -> error $ "Failed to parse " ++ show x

twitchServer = "irc.chat.twitch.tv"
twitchPort = 6667

connectTwitch :: IO Handle
connectTwitch = connectTo twitchServer (PortNumber twitchPort)

talk :: String -> String -> String -> IO ()
talk twitchUser twitchToken twitchChannel = do
    putStrLn "Loading saved queue file"
    maybeQueue <- loadMostRecentQueue
    queue <- case maybeQueue of
        Left errorMessage -> putStrLn errorMessage >> return defaultQueue
        Right q -> return q
    conn <- connectTwitch
    hSetBuffering conn NoBuffering
    write conn "PASS" twitchToken
    write conn "NICK" twitchUser
    write conn "JOIN" twitchChannel
    replicateM_ 10 $ hGetLine conn >>= putStrLn
    write conn "PRIVMSG" (twitchChannel ++ " :Hi everybody")
    handleMessages twitchChannel conn queue

handleMessages :: String -> Handle -> Queue -> IO ()
handleMessages twitchChannel conn q = do
    nextLine <- hGetLine conn
    q' <- handleMessage twitchChannel conn nextLine q
    handleMessages twitchChannel conn q'

handleMessage :: String -> Handle -> String -> Queue -> IO Queue
handleMessage twitchChannel conn msg q = do
    time <- getCurrentTime
    putStrLn msg
    case parseMessage msg of
        (Message (Just userString) "PRIVMSG" (_:params)) -> do
            let user = TwitchUser userString
            let cmd = parseCommand user (reverse . drop 1 . reverse . unwords $ params)
            let (q', maybeReturnMessage) = handleTimestamped user time cmd q
            when (queueIndex q /= queueIndex q') (snapshotQueue q')
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
