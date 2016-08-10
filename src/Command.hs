module Command where

import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Data.Char

data Mode = Singles | Doubles | Crew | InvalidMode String deriving (Show, Eq, Ord)
newtype NNID = NNID { getNNID :: String } deriving (Show, Eq, Ord)
newtype MiiName = MiiName { getMiiName :: String } deriving (Show, Eq, Ord)
newtype TwitchUser = TwitchUser { getTwitchUser :: String } deriving (Show, Eq, Ord)
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
    | Leave TwitchUser
    | Remove UserOrTeam
    | Info TwitchUser
    | Enter TwitchUser
    | Here TwitchUser
    -- team
    | NewTeam TwitchUser TeamName
    | TeamInvite TwitchUser TwitchUser
    | Accept TwitchUser TeamName
    | Decline TwitchUser TeamName
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

cmd prefix parser help = CommandSpec prefix (const parser) help False
restrictedCmd prefix parser help = CommandSpec prefix (const parser) help True
userCmd prefix parser help = CommandSpec prefix parser help False

commands =
    [ restrictedCmd "smash on" (return On) "Turns the bot on"
    , restrictedCmd "smash off" (return Off) "Turns the bot off"
    , restrictedCmd "smash mode" (Mode <$> modeP) "Sets the current mode"
    , restrictedCmd "smash allow" (Allow <$> userP)
        "Allows given user to use restricted commands"
    , restrictedCmd "smash deny" (Deny <$> userP)
        "Takes away users right te use restricted commands"
    , restrictedCmd "smash restrict" (Restrict <$> remainderP)
        "Restricts the given command"
    , restrictedCmd "smash unrestrict" (Unrestrict <$> remainderP)
        "Makes the given command not restricted. Doesn't work on default restricted commands."
    , restrictedCmd "smash new" (return New) "Creates a new queue"
    , restrictedCmd "smash open" (return Open) "Opens the queue to entrants"
    , restrictedCmd "smash close" (return Close) "Closes the queue to entrants"
    , restrictedCmd "smash start" (return Start) "Starts queueing users/teams"
    , restrictedCmd "win" (return Win)
        "Mark the given user/team as a winner. Defaults to streamer"
    , restrictedCmd "lose" (return Lose)
        "Mark the opposing team/user as the winner"
    , restrictedCmd "smash skip" (return Skip)
        "Skip the current team/user in the queue"
    , restrictedCmd "smash move" (Move <$> userOrTeamP <*> intP)
        "Move the given user/team to the given position in the queue"
    , restrictedCmd "cb" (SetCrewStock <$> crewP <*> intP)
        "Sets the crew stock for the given crew (e. g. !cb a 4)"
    , restrictedCmd "friendme list" (FriendList <$> maybeP intP)
        "List friendme's in the queue, with optional limit"
    , restrictedCmd "friendme clear" (FriendClear <$> maybeP intP)
        "Clear friendme's in the queue, with optional limit"
    , cmd "help" (Help <$> remainderP)
        "Print help for the given command"
    , userCmd "nnid" (return . GetNNID)
        "Display the nnid qbot has for you"
    , userCmd "index" (\user -> Index user <$> nnidP <*> miiP)
        "Add yourself to the index with given nnid and miiname"
    , userCmd "friendme" (return . Friend)
        "Mark yourself as needing to be friended"
    , cmd "list" (List <$> maybeP intP)
        "Display the current queue list with the given limit (default 10)"
    , cmd "ruleset" (RuleSet <$> maybeP modeP)
        "Display the ruleset for the given mode (default current mode)"
    , userCmd "leave" (return . Leave)
        "Remove yourself from the queue"
    , restrictedCmd "remove" (Remove <$> userOrTeamP)
        "Remove given user from the queue"
    , userCmd "info" (\user -> Info <$> option user userP)
        "Display info for the given user (default to yourself)"
    , userCmd "enter" (return . Enter)
        "Enter yourself into the queue"
    , userCmd "here" (return . Here)
        "Mark yourself as here for rollcall"
    , userCmd "teamcreate" (\user -> NewTeam user <$> teamP)
        "Create a new team with the given name"
    , userCmd "teaminv" (\user -> TeamInvite user <$> userP)
        "Invite the given user to your team"
    , userCmd "accept" (\user -> Accept user <$> teamP)
        "Accept an invitation to the given team"
    , userCmd "decline" (\user -> Decline user <$> teamP)
        "Decline an invitation to the given team"
    , userCmd "teamleave" (return . LeaveTeam)
        "Leave your current team"
    , cmd "stocks" (return CrewStocks)
        "Display the current crew stocks"
    , userCmd "joincrewa" (\user -> return (JoinCrew user A))
        "Join crew a"
    , userCmd "joincrewb" (\user -> return (JoinCrew user B))
        "Join crew b"
    , restrictedCmd "streamer" (Streamer <$> userP)
        "Sets the current streamer"
    , restrictedCmd "denyreply" (DenyReply <$> remainderP)
        "Sets the access denied reply"
    , restrictedCmd "winbuffer" (WinBuffer <$> intOrOffP)
        "Sets the win buffer, minimum number of seconds between !win commands (can be a number or off)"
    , restrictedCmd "listbuffer" (ListBuffer <$> intOrOffP)
        "Sets the list buffer, minimum number of seconds between !list commands (can be a number or off)"
    , restrictedCmd "singleslimit" (SinglesLimit <$> intOrOffP)
        "Sets the max queue size for singles (can be a number or off)"
    , restrictedCmd "doubleslimit" (DoublesLimit <$> intOrOffP)
        "Sets the max queue size for doubles (can be a number or off)"
    , restrictedCmd "reenterwait" (ReenterWait <$> intOrOffP)
        "Sets the number of minutes a player must wait before re-entering a queue (can be a number or off)"
    , restrictedCmd "herealert" (HereTimeout <$> intOrOffP)
        "Sets the number of seconds a player has to respond to rollcall with !here (can be a number or off)"
    , restrictedCmd "singlesbestof" (BestOf Singles <$> intP)
        "Sets the \"best of\" for a singles set"
    , restrictedCmd "doublesbestof" (BestOf Doubles <$> intP)
        "Sets the \"best of\" for a doubles set"
    , restrictedCmd "settings" (return ShowSettings)
        "Display all settings"
    ]

commandMap :: Map.Map String CommandSpec
commandMap = Map.fromList . fmap (\c -> (commandSpecPrefix c, c)) $ commands

commandP user = do
    _ <- char '!'
    prefix <- choice (fmap string . Map.keys $ commandMap)
    let commandSpec = commandMap Map.! prefix
    result <- commandSpecParser commandSpec user
    many (char ' ')
    eof
    return (commandSpec, result)

modeP = spaceP >> choice
    [ mapStringP "singles" Singles
    , mapStringP "doubles" Doubles
    , mapStringP "cb" Crew
    , many1 get >>= \m -> eof >> return (InvalidMode m)
    ]
mapStringP s r = string s >> return r
userP = typedIdentifierP TwitchUser
nnidP = typedIdentifierP NNID
miiP = fmap MiiName remainderP
userOrTeamP = typedIdentifierP UserOrTeam
teamP = typedIdentifierP TeamName
typedIdentifierP t = spaceP >> (fmap t $ many1 (satisfy (/=' ')))
spaceP = many1 (char ' ')
intP = do
    spaceP
    minus <- option "" (string "-")
    digits <- many1 (satisfy isDigit)
    return (read $ minus ++ digits)
crewP = spaceP >> choice [char 'a' >> return A, char 'b' >> return B]
maybeP p = option Nothing (fmap return p)
intOrOffP = choice [spaceP >> string "off" >> return Nothing, Just <$> intP]
remainderP = spaceP >> many1 get

parseCommand :: TwitchUser -> String -> (CommandSpec, Command)
parseCommand user commandString = case readP_to_S (commandP user) commandString of
    ((cmd,_):_) -> cmd
    _ -> (InvalidCommandSpec, Invalid commandString)
