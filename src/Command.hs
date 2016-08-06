module Command where

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
    | Win (Maybe UserOrTeam)
    | Lose
    | Skip
    | Move UserOrTeam Position
    | SetCrewStock CrewSide Int
    | FriendList (Maybe Position)
    | FriendClear (Maybe Position)
    -- general
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
    | Next
    -- team
    | NewTeam TwitchUser TeamName
    | TeamInvite TwitchUser TwitchUser
    | Accept TwitchUser
    | Decline TwitchUser
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

commandP user = char '!' >> choice
    [ onP
    , offP
    , setModeP
    , allowP
    , denyP
    , restrictP
    , unrestrictP
    , newP
    , openP
    , closeP
    , startP
    , winP
    , loseP
    , skipP
    , moveP
    , setCrewStockP
    , friendListP
    , friendClearP
    , getNNIDP user
    , indexP user
    , friendP user
    , listP
    , ruleSetP
    , leaveP user
    , removeP
    , infoP user
    , enterP user
    , hereP user
    , nextP
    , newTeamP user
    , teamInviteP user
    , acceptP user
    , declineP user
    , leaveTeamP user
    , crewStocksP
    , joinCrewP user
    , streamerP
    , denyReplyP
    , winBufferP
    , listBufferP
    , singlesLimitP
    , doublesLimitP
    , reenterWaitP
    , hereTimeoutP
    , bestOfP
    , showSettingsP
    ] >>= \cmd -> eof >> return cmd
onP = mapSmashP "on" On
offP = mapSmashP "off" Off
setModeP = smashCmdP "mode" >> spaceP >> Mode <$> modeP
modeP = choice
    [ mapStringP "singles" Singles
    , mapStringP "doubles" Doubles
    , mapStringP "cb" Crew
    , many1 get >>= \m -> eof >> return (InvalidMode m)
    ]
allowP = smashCmdP "allow" >> spaceP >> Allow <$> userP
denyP = smashCmdP "deny" >> spaceP >> Deny <$> userP
restrictP = smashCmdP "restrict" >> spaceP >> Restrict <$> (many1 get)
unrestrictP = smashCmdP "unrestrict" >> spaceP >> Unrestrict <$> (many1 get)
newP = mapSmashP "new" New
openP = mapSmashP "open" Open
closeP = mapSmashP "close" Close
startP = mapSmashP "start" Start
winP = string "win" >> optional spaceP >> Win <$> maybeP userOrTeamP
loseP = string "lose" >> return Lose
skipP = mapSmashP "skip" Skip
moveP = smashCmdP "move" >> Move <$> spaced userOrTeamP <*> spaced intP
setCrewStockP = mapStringP "cb" SetCrewStock <*> spaced crewP <*> spaced intP
friendListP = mapStringP "friendme list" FriendList <*> maybeSpaced (maybeP intP)
friendClearP = mapStringP "friendme clear" FriendClear <*> maybeSpaced (maybeP intP)
getNNIDP user = mapStringP "nnid" (GetNNID user)
indexP user = mapStringP "index" (Index user) <*> spaced nnidP <*> spaced miiP
friendP user = mapStringP "friendme" (Friend user)
listP = mapStringP "list" List <*> maybeSpaced (maybeP intP)
ruleSetP = mapStringP "ruleset" RuleSet <*> maybeSpaced (maybeP modeP)
leaveP user = mapStringP "leave" (Leave user)
removeP = mapStringP "remove" Remove <*> spaced userOrTeamP
infoP user = mapStringP "info" Info <*> option user (spaced userP)
enterP user = mapStringP "enter" (Enter user)
hereP user = mapStringP "here" (Here user)
nextP = mapStringP "next" Next
newTeamP user = mapStringP "teamcreate" (NewTeam user) <*> spaced teamP
teamInviteP user = mapStringP "teaminv" (TeamInvite user) <*> spaced userP
acceptP user = mapStringP "accept" (Accept user)
declineP user = mapStringP "decline" (Decline user)
leaveTeamP user = mapStringP "teamleave" (LeaveTeam user)
crewStocksP = mapStringP "stocks" CrewStocks
joinCrewP user = mapStringP "joincrew" (JoinCrew user) <*> crewP
streamerP = mapStringP "streamer" Streamer <*> spaced userP
denyReplyP = mapStringP "denyreply" DenyReply <*> spaced (many1 get)
winBufferP = mapStringP "winbuffer" WinBuffer <*> spaced intOrOffP
listBufferP = mapStringP "listbuffer" ListBuffer <*> spaced intOrOffP
singlesLimitP = mapStringP "singleslimit" SinglesLimit <*> spaced intOrOffP
doublesLimitP = mapStringP "doubleslimit" DoublesLimit <*> spaced intOrOffP
reenterWaitP = mapStringP "reenterwait" ReenterWait <*> spaced intOrOffP
hereTimeoutP = mapStringP "herealert" HereTimeout <*> spaced intOrOffP
bestOfP = choice
    [ mapStringP "singlesbestof" (BestOf Singles)
    , mapStringP "doublesbestof" (BestOf Doubles)
    ] <*> spaced intP
showSettingsP = mapStringP "settings" ShowSettings

mapSmashP cmdString cmd = smashCmdP cmdString >> return cmd
mapStringP s r = string s >> return r
smashCmdP cmd = string "smash" >> spaceP >> string cmd
userP = fmap TwitchUser $ many1 (satisfy (/=' '))
nnidP = fmap NNID $ many1 (satisfy (/=' '))
miiP = fmap MiiName $ many1 (satisfy (/=' '))
userOrTeamP = fmap UserOrTeam $ many1 (satisfy (/=' '))
teamP = fmap TeamName $ many1 (satisfy (/=' '))
spaceP = many1 (char ' ')
intP = do
    minus <- option "" (string "-")
    digits <- many1 (satisfy isDigit)
    return (read $ minus ++ digits)
crewP = choice [char 'a' >> return A, char 'b' >> return B]
maybeP p = option Nothing (fmap return p)
spaced p = spaceP >> p
maybeSpaced p = many (char ' ') >> p
intOrOffP = choice [string "off" >> return Nothing, Just <$> intP]

parseCommand :: TwitchUser -> String -> Maybe Command
parseCommand user commandString = case readP_to_S (commandP user) commandString of
    ((cmd,_):_) -> Just cmd
    _ -> Nothing
