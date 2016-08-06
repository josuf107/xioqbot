module Lib where

import Control.Concurrent
import Control.Monad
import Data.Char
import Network
import System.IO
import Text.Printf
import Text.ParserCombinators.ReadP

data Message
    = Message
    { messageCommand :: String
    , messageParams :: [String]
    } deriving (Show,Ord,Eq)

messageP :: ReadP Message
messageP = do
    char ':'
    prefixP
    spaceP
    command <- commandP
    params <- paramsP
    return (Message command params)
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
    forever $ hGetLine conn >>= handleMessage conn

handleMessage :: Handle -> String -> IO ()
handleMessage conn msg = do
    putStrLn msg
    let parsedMessage = parseMessage msg
    case parsedMessage of
        (Message "PRIVMSG" (_:params)) -> do
            write conn "PRIVMSG" (twitchChannel ++ " :" ++ unwords params)
        _ -> return ()

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t
