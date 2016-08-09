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
twitchToken = "oauth:tw3kl6ia2b7cwe8brhje3lly81t3ro"
twitchUser = "xioqbot"
twitchChannel = "#josuf107"

connectTwitch :: IO Handle
connectTwitch = connectTo twitchServer (PortNumber twitchPort)

talk :: IO ()
talk = do
    conn <- connectTwitch
    hSetBuffering conn NoBuffering
    write conn "PASS" twitchToken
    write conn "NICK" twitchUser
    write conn "JOIN" twitchChannel
    replicateM_ 10 $ hGetLine conn >>= putStrLn
    write conn "PRIVMSG" (twitchChannel ++ " :Hi everybody")
    handleMessages conn defaultQueue

handleMessages :: Handle -> Queue -> IO ()
handleMessages conn q = do
    nextLine <- hGetLine conn
    q' <- handleMessage conn nextLine q
    handleMessages conn q'

handleMessage :: Handle -> String -> Queue -> IO Queue
handleMessage conn msg q = do
    time <- getCurrentTime
    putStrLn msg
    case parseMessage msg of
        (Message (Just userString) "PRIVMSG" (_:params)) -> do
            let user = TwitchUser userString
            let cmd = parseCommand user (reverse . drop 1 . reverse . unwords $ params)
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
