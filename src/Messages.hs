module Messages where

import Command
import Queue

import Prelude hiding (print)
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type Msg a = a -> State Queue (Maybe String)
type Msg_ = State Queue (Maybe String)

class Print a where
    print :: Msg a
    printList :: Msg [a]
    printList as = do
        maybeStrings <- mapM print as
        msg . intercalate ", " . catMaybes $ maybeStrings

instance Print Mode where
    print = msg . show

instance Print Char where
    print = msg . return
    printList as = do
        maybeStrings <- mapM print as
        msg . concat . catMaybes $ maybeStrings

instance Print a => Print [a] where
    print = printList

instance Print a => Print (Seq.Seq a) where
    print as = do
        maybeStrings <- mapM print as
        msg . intercalate ", " . catMaybes . foldr (:) [] $ maybeStrings

instance Print a => Print (Maybe a) where
    print Nothing = msg ""
    print (Just a) = print a

instance Print TwitchUser where
    print = msg . getTwitchUser

instance Print MiiName where
    print = msg . getMiiName
    printList = msg . intercalate " & " . fmap getMiiName

instance Print NNID where
    print = msg . getNNID

instance Print TeamName where
    print = msg . getTeamName

instance Print UserOrTeam where
    print = printUserOrTeam

instance Print CrewSide where
    print A = msg "A"
    print B = msg "B"

instance Print Int where
    print = msg . show

(&) :: (Print a, Print b) => a -> b -> State Queue (Maybe String)
a & b = do
    maybePart1 <- print a
    maybePart2 <- print b
    msg . concat . catMaybes $ [maybePart1, maybePart2]

(%) :: (Print a) => State Queue (Maybe String) -> a -> State Queue (Maybe String)
s % b = do
    maybePart1 <- s
    maybePart2 <- print b
    msg . concat . catMaybes $ [maybePart1, maybePart2]

msg :: String -> State Queue (Maybe String)
msg m = return (Just m)

noMsg :: State Queue (Maybe String)
noMsg = return Nothing

helpMsg :: Msg (Maybe String, String)
helpMsg (Just helpString, _) = msg helpString
helpMsg (Nothing, which) = which & " is not a valid command"

invalidMsg :: Msg String
invalidMsg which = which & " is not a valid command"

disabledMsg :: Msg_
disabledMsg = msg "qbot disabled. !smash on to re-enable"

modeMsg :: Msg Mode
modeMsg newMode = "New mode is: " & newMode

allowMsg :: Msg TwitchUser
allowMsg user = "Added admin: " & user

denyMsg :: Msg TwitchUser
denyMsg user = "Removed admin: " & user

restrictMsg :: Msg String
restrictMsg command = "Restricted command: " & command

unrestrictMsg :: Msg String
unrestrictMsg command = "Unrestricted command: " & command

newQueueMsg :: Msg_
newQueueMsg = msg "Created a new queue!"

openMsg :: Msg_
openMsg = msg "The queue is open! Type !enter to enter"

closeMsg :: Msg_
closeMsg = msg "The queue is closed"

softCloseMsg :: Msg_
softCloseMsg = msg "The queue is closed to repeat entrants"

startMsg :: Msg (Maybe UserOrTeam, Maybe UserOrTeam)
startMsg (Just current, Just next) = "The first match is beginning and the opponent is " & current % "! Next up is " % next % "!"
startMsg (Just current, Nothing) = "The first match is beginning and the opponent is " & current % "!"
startMsg (Nothing, _) = msg "Can't start because the queue is empty!"

setWinMsg :: Msg (UserOrTeam, UserOrTeam, Int, Int, Maybe UserOrTeam)
setWinMsg (winner, loser, setWins, setLosses, (Just next)) = winner
    & " has won the set against "
    % loser
    % "! The score was " % setWins % ":" % setLosses % "."
    % " Next up is " % next % "!"
setWinMsg (winner, loser, setWins, setLosses, Nothing) = winner
    & " has won the set against "
    % loser
    % "! The score was " % setWins % ":" % setLosses % "."

winMsg :: Msg (UserOrTeam, UserOrTeam, Int, Int, Int)
winMsg (winner, loser, setWins, setLosses, required) = winner
    & " has just won a match against "
    % loser
    % "! The score is " % setWins % ":" % setLosses
    % " and it requires " % required % " to take the set"

emptyQueueMsg :: Msg_
emptyQueueMsg = msg "There's no one in the queue"

streamerNotOnATeamWinMsg :: Msg_
streamerNotOnATeamWinMsg = msg "The streamer is not on a team so I can't mark a win"

streamerNotOnATeamLossMsg :: Msg_
streamerNotOnATeamLossMsg = msg "The streamer is not on a team so I can't mark a loss"

skipMsg :: Msg (Maybe UserOrTeam, Maybe UserOrTeam)
skipMsg (Nothing, _) = msg "The queue is empty. No one to skip"
skipMsg (Just skipped, Nothing) = "Skipped " & skipped % ". The queue is now empty"
skipMsg (Just skipped, Just next) = "Skipped " & skipped
    % ". Next up is " % next % "!"

moveMsg :: Msg (UserOrTeam, Int)
moveMsg (target, rank) = "Moved " & target % " to position " % rank % " in the queue"

setCrewStockMsg :: Msg (CrewSide, Int)
setCrewStockMsg (crew, stock) = "Set crew stock for crew " & crew % " to " % stock

friendListMsg :: Msg (Int, Seq.Seq UserOrTeam)
friendListMsg (limit, friendMes) = "Next " & limit % " friendmes in the queue: "
    % friendMes

friendListClearMsg :: Msg Int
friendListClearMsg limit = "Cleared friend list with limit " & limit

getNNIDMsg :: Msg (TwitchUser, Maybe (NNID, a, b, c))
getNNIDMsg (user, Just (nnid, _, _, _)) = user & "'s NNID is " % nnid % "."
getNNIDMsg (user, Nothing) = user & " is not in the index. Try !index NNID MiiName."

indexMsg :: Msg TwitchUser
indexMsg user = "Added " & user % " to index"

friendMeMsg :: Msg TwitchUser
friendMeMsg user = "Added " & user % " to friendme list"

listMsg :: Msg (Maybe UserOrTeam, Seq.Seq UserOrTeam)
listMsg (Nothing, _) = msg "Queue is empty."
listMsg (Just current, remainder) = 
    case Seq.null remainder of
        True -> "Currently playing " & current % ". No other entries in the queue."
        False -> "Currently playing " & current % ". Next in queue: " % remainder

ruleSetMsg :: Msg (Mode, String)
ruleSetMsg (mode, ruleset) = mode & " Ruleset: " % ruleset

stageListMsg :: Msg String
stageListMsg stagelist = "Legal Stages: " & stagelist

unableToRemoveMsg :: Msg TwitchUser
unableToRemoveMsg user = user & " is not on a team, so I can't remove them from the queue."

notInTheQueueMsg :: Msg UserOrTeam
notInTheQueueMsg userOrTeam = userOrTeam & " is not in the queue, so I can't remove them from the queue."

removedMsg :: Msg UserOrTeam
removedMsg userOrTeam = "Removed " & userOrTeam % " from the queue."

infoMsg :: Msg (TwitchUser, Int, Maybe (NNID, MiiName, Int, Int), Maybe Int)
infoMsg (user, _, Nothing, _) = user & " is not in the index. Add yourself with !index NNID MiiName."
infoMsg (user, _, Just (nnid, miiName, wins, losses), Nothing) =
    "| User: " & user
    % " | NNID: " % nnid
    % " | MiiName: " % miiName
    % " | W:L " % wins % ":" % losses % " |"
infoMsg (user, queueSize, Just (nnid, miiName, wins, losses), Just position) =
    "| User: " & user
    % " | Position " % position % "/" % queueSize % " in Queue"
    % " | NNID: " % nnid
    % " | MiiName: " % miiName
    % " | W:L " % wins % ":" % losses % " |"

enterMsg :: Msg (TwitchUser, TwitchUser, Bool, Bool, Maybe UserOrTeam, Bool, Bool, Int)
enterMsg (streamer, user, open, indexed, maybeUserOrTeam, alreadyInQueue, userOrTeamSoftClosed, position) =
    case (open, indexed, maybeUserOrTeam, alreadyInQueue, userOrTeamSoftClosed) of
        (False, _, _, _, _) -> msg "Sorry the queue is closed so you can't !enter. An admin must use !smash open to open the queue."
        (_, False, _, _, _) -> user & " is not in the index. Add yourself with !index NNID MiiName."
        (_, _, Nothing, _, _) -> "Couldn't add " & user % " to queue. Try joining a team."
        (_, _, Just userOrTeam, True, _) -> "Sorry " & userOrTeam
            % ", you can't join the queue more than once!"
        (_, _, Just userOrTeam, _, True) -> "Sorry " & userOrTeam
            % ", you can't join the queue again because it is soft closed!"
        (_, _, Just userOrTeam, _, False) -> userOrTeam
            & ", you've now been placed into the queue at position "
            % position
            % "! Type !info to see your position and !friendme if you've yet to add "
            % streamer % "."

hereMsg :: Msg TwitchUser
hereMsg user = "Okay! " & user % " is ready!"

alreadyOnTeamMsg :: Msg (TwitchUser, TeamName)
alreadyOnTeamMsg (creator, existingTeam) = "Sorry " & creator % ", you are already in a team! You can leave your current team (" % existingTeam % ") with !teamleave."

newTeamMsg :: Msg (TwitchUser, TeamName)
newTeamMsg (creator, team) = creator & " has created the new team "
    % team % "! Invite an ally with !teaminv [name]."

-- Message Utils

printUserOrTeam :: Msg UserOrTeam
printUserOrTeam userOrTeam = do
    friendMes <- fmap (Map.member userOrTeam) getFriendMesForMode
    miiNames <- getMiiNames userOrTeam
    let displayUserOrTeam = getUserOrTeam userOrTeam
    case (friendMes, miiNames) of
        (True, []) -> displayUserOrTeam & "+"
        (False, []) -> print displayUserOrTeam
        (True, miis) -> displayUserOrTeam & "+ (" % miis % ")"
        (False, miis) -> displayUserOrTeam & " (" % miis % ")"
