module Command where

import Text.ParserCombinators.ReadP

data Mode = Singles | Doubles | Crew | InvalidMode String deriving (Show, Eq, Ord)
newtype NNID = NNID { getNNID :: String } deriving (Show, Eq, Ord)
newtype MiiName = MiiName { getMiiName :: String } deriving (Show, Eq, Ord)
newtype TwitchUser = TwitchUser { getTwitchUser :: String } deriving (Show, Read, Eq, Ord)
newtype TeamName = TeamName { getTeamName :: String } deriving (Show, Eq, Ord)
newtype UserOrTeam = UserOrTeam { getUserOrTeam :: String } deriving (Show, Eq, Ord)
data CrewSide = A | B deriving (Show, Eq, Ord)
type Position = Int
type Seconds = Int
type Minutes = Int
data CommandSpec
    = CommandSpec
    { commandSpecPrefix :: String
    , commandSpecParser :: TwitchUser -> ReadP Command
    , commandSpecHelp :: String
    , commandSpecDefaultRestrict :: Bool
    }
    | InvalidCommandSpec

data Command
    -- admin
    = On
    | Off
    | Mode Mode
    | Allow TwitchUser
    | Deny TwitchUser
    | Restrict String -- This is for making a public command restricted
    | Unrestrict String  -- This is for making a restricted command public, but shouldn't work fer default restricted commands
    | New
    | Open
    | Close
    | SoftClose
    | Start
    | Win
    | Lose
    | Skip
    | Move UserOrTeam Position
    | SetCrewStock CrewSide Int
    | FriendList (Maybe Position)
    | FriendClear (Maybe Position)
    -- general
    | Help String
    | GetNNID TwitchUser
    | Index TwitchUser NNID MiiName
    | Friend TwitchUser
    | List (Maybe Int)
    | RuleSet (Maybe Mode)
    | Stagelist
    | Leave TwitchUser
    | Remove UserOrTeam
    | Info TwitchUser
    | Enter TwitchUser
    | Here TwitchUser
    -- team
    | NewTeam TwitchUser TeamName
    | TeamInvite TwitchUser TwitchUser
    | Accept TwitchUser (Maybe TeamName)
    | Decline TwitchUser TeamName
    | TeamInfo TeamName
    | LeaveTeam TwitchUser
    -- crew
    | CrewStocks
    | JoinCrew TwitchUser CrewSide
    -- settings
    | Streamer TwitchUser
    | DenyReply String
    | WinBuffer (Maybe Seconds)
    | ListBuffer (Maybe Seconds)
    | SinglesLimit (Maybe Int)
    | DoublesLimit (Maybe Int)
    | ReenterWait (Maybe Minutes)
    | HereTimeout (Maybe Seconds)
    | BestOf Mode Int
    | ShowSettings
    -- Parse error
    | Invalid String
    deriving (Show, Eq, Ord)
