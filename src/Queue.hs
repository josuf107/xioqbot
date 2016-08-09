-- This is the state data structure and the implementation of the commands on
-- it
module Queue where

import Command

import Control.Monad
import Control.Monad.State
import Data.Tuple
import Data.Time
import Data.Maybe
import Data.Monoid
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Text.Printf

test :: IO ()
test = go defaultQueue
    where
        go q = do
            input <- getLine
            let overrideUser = take 1 input /= "!"
            let maybeUser = head . words $ input
            let user = TwitchUser $ if overrideUser then maybeUser else "josuf107"
            let cmdString = unwords . (if overrideUser then drop 1 else id) . words $ input
            let cmd = parseCommand user cmdString
            let (q', maybeReturnMessage) = handleMaybeDisabled user cmd q
            print maybeReturnMessage
            go q'

data Queue
    = Queue
    { queueOn :: Bool
    , queueLastMessage :: UTCTime
    , queueMode :: Mode
    , queueAdmins :: Set.Set TwitchUser
    , queueRestricted :: Set.Set String
    , queueQueue :: Seq.Seq UserOrTeam
    , queueOpen :: Bool
    , queueTeams :: Map.Map TwitchUser TeamName
    , queueStreamer :: TwitchUser
    , queueSetWins :: Int
    , queueSetLosses :: Int
    , queueCrewStockA :: Int
    , queueCrewStockB :: Int
    , queueFriendMes :: Set.Set TwitchUser
    , queueIndex :: Map.Map TwitchUser (NNID, MiiName, Int, Int)
    , queueRulesSingles :: String
    , queueRulesDoubles :: String
    , queueRulesCrew :: String
    , queueHereMap :: Map.Map TwitchUser UTCTime
    , queueInvites :: Map.Map TwitchUser (Set.Set TeamName)
    , queueDenyReply :: String
    , queueWinBuffer :: Maybe Seconds
    , queueListBuffer :: Maybe Seconds
    , queueSinglesLimit :: Maybe Int
    , queueDoublesLimit :: Maybe Int
    , queueReenterWait :: Maybe Minutes
    , queueHereAlert :: Maybe Seconds
    , queueSinglesBestOf :: Int
    , queueDoublesBestOf :: Int
    } deriving (Show, Eq, Ord)
type Message = String

defaultQueue = Queue
    { queueOn = True
    , queueLastMessage = UTCTime (fromGregorian 2016 1 1) 0
    , queueMode = Singles
    , queueAdmins = Set.fromList [TwitchUser "josuf107"]
    , queueRestricted = Set.empty
    , queueQueue = Seq.empty
    , queueOpen = False
    , queueTeams = Map.empty
    , queueStreamer = TwitchUser "josuf107"
    , queueSetWins = 0
    , queueSetLosses = 0
    , queueCrewStockA = 3
    , queueCrewStockB = 3
    , queueFriendMes = Set.empty
    , queueIndex = Map.empty
    , queueRulesSingles = "It's singles."
    , queueRulesDoubles = "It's doubles."
    , queueRulesCrew = "It's a crew battle."
    , queueHereMap = Map.empty
    , queueInvites = Map.empty
    , queueDenyReply = "You can't do that"
    , queueWinBuffer = Nothing
    , queueListBuffer = Nothing
    , queueSinglesLimit = Nothing
    , queueDoublesLimit = Nothing
    , queueReenterWait = Nothing
    , queueHereAlert = Nothing
    , queueSinglesBestOf = 3
    , queueDoublesBestOf = 3
    }

type Modify a = a -> a
type QueueModify a = Modify a -> State Queue ()
type QueueSet a = a -> State Queue ()

setQueueOn :: QueueSet Bool
setQueueOn v = modify $ \q -> q { queueOn = v }
setQueueMode :: QueueSet Mode
setQueueMode v = modify $ \q -> q { queueMode = v }
withQueueAdmins :: QueueModify (Set.Set TwitchUser)
withQueueAdmins f = modify $ \q -> q { queueAdmins = f (queueAdmins q) }
withQueueRestricted :: QueueModify (Set.Set String)
withQueueRestricted f = modify $ \q -> q { queueRestricted = f (queueRestricted q) }
withQueueQueue :: QueueModify (Seq.Seq UserOrTeam)
withQueueQueue f = modify $ \q -> q { queueQueue = f (queueQueue q) }
setQueueOpen :: QueueSet Bool
setQueueOpen v = modify $ \q -> q { queueOpen = v }
withQueueTeams :: QueueModify (Map.Map TwitchUser TeamName)
withQueueTeams f = modify $ \q -> q { queueTeams = f (queueTeams q) }
withQueueSetWins :: QueueModify Int
withQueueSetWins f = modify $ \q -> q { queueSetWins = f (queueSetWins q) }
withQueueSetLosses :: QueueModify Int
withQueueSetLosses f = modify $ \q -> q { queueSetLosses = f (queueSetLosses q) }
setQueueCrewStockA :: QueueSet Int
setQueueCrewStockA v = modify $ \q -> q { queueCrewStockA = v }
setQueueCrewStockB :: QueueSet Int
setQueueCrewStockB v = modify $ \q -> q { queueCrewStockB = v }
withQueueFriendMes :: QueueModify (Set.Set TwitchUser)
withQueueFriendMes f = modify $ \q -> q { queueFriendMes = f (queueFriendMes q) }
withQueueIndex :: QueueModify (Map.Map TwitchUser (NNID, MiiName, Int, Int))
withQueueIndex f = modify $ \q -> q { queueIndex = f (queueIndex q) }
setQueueRulesSingles :: QueueSet String
setQueueRulesSingles v = modify $ \q -> q { queueRulesSingles = v }
setQueueRulesDoubles :: QueueSet String
setQueueRulesDoubles v = modify $ \q -> q { queueRulesDoubles = v }
setQueueRulesCrew :: QueueSet String
setQueueRulesCrew v = modify $ \q -> q { queueRulesCrew = v }
withQueueHereMap :: QueueModify (Map.Map TwitchUser UTCTime)
withQueueHereMap f = modify $ \q -> q { queueHereMap = f (queueHereMap q) }
withQueueInvites :: QueueModify (Map.Map TwitchUser (Set.Set TeamName))
withQueueInvites f = modify $ \q -> q { queueInvites = f (queueInvites q) }
setQueueStreamer :: QueueSet TwitchUser
setQueueStreamer v = modify $ \q -> q { queueStreamer = v }
setQueueDenyReply :: QueueSet String
setQueueDenyReply v = modify $ \q -> q { queueDenyReply = v }
setQueueWinBuffer :: QueueSet (Maybe Seconds)
setQueueWinBuffer v = modify $ \q -> q { queueWinBuffer = v }
setQueueListBuffer :: QueueSet (Maybe Seconds)
setQueueListBuffer v = modify $ \q -> q { queueListBuffer = v }
setQueueSinglesLimit :: QueueSet (Maybe Int)
setQueueSinglesLimit v = modify $ \q -> q { queueSinglesLimit = v }
setQueueDoublesLimit :: QueueSet (Maybe Int)
setQueueDoublesLimit v = modify $ \q -> q { queueDoublesLimit = v }
setQueueReenterWait :: QueueSet (Maybe Minutes)
setQueueReenterWait v = modify $ \q -> q { queueReenterWait = v }
setQueueHereAlert :: QueueSet (Maybe Seconds)
setQueueHereAlert v = modify $ \q -> q { queueHereAlert = v }
setQueueSinglesBestOf :: QueueSet Int
setQueueSinglesBestOf v = modify $ \q -> q { queueSinglesBestOf = v }
setQueueDoublesBestOf :: QueueSet Int
setQueueDoublesBestOf v = modify $ \q -> q { queueDoublesBestOf = v }

handleTimestamped :: TwitchUser -> UTCTime -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleTimestamped user time cmd q = handleMaybeDisabled user cmd (q { queueLastMessage = time })

handleMaybeDisabled :: TwitchUser -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleMaybeDisabled user (_, On) q = (q { queueOn = True }, Just "qbot enabled. Hello everyone!")
handleMaybeDisabled user cmd q = if queueOn q then handleMaybeRestricted user cmd q else (q, Nothing)

handleMaybeRestricted :: TwitchUser -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleMaybeRestricted user (InvalidCommandSpec, cmd) q = (q, Just "")
handleMaybeRestricted user (spec, cmd) q =
    if (commandSpecDefaultRestrict spec
        || commandSpecPrefix spec `Set.member` (queueRestricted q))
        && user `Set.notMember` (queueAdmins q)
    then
        (q, Just $ getTwitchUser user ++ " is not allowed to perform command: " ++ commandSpecPrefix spec)
    else swap (runState (handleCommand cmd) q)

handleCommand :: Command -> State Queue (Maybe Message)
handleCommand (Help which) = return . Just $ (maybe
    (which ++ " is not a valid command")
    commandSpecHelp
    (Map.lookup which commandMap))
handleCommand (Invalid str) = return . Just $ (str ++ " is not a valid command")
handleCommand Off = do
    setQueueOn False
    msg "qbot disabled. !smash on to re-enable"
handleCommand (Mode mode) = do
    setQueueMode mode
    msg $ printf "New mode is: %s" (show mode)
handleCommand (Allow user) = do
    withQueueAdmins (Set.insert user)
    msg $ printf "Added admin: %s" (getTwitchUser user)
handleCommand (Deny user) = do
    withQueueAdmins (Set.delete user)
    msg $ printf "Removed admin: %s" (getTwitchUser user)
handleCommand (Restrict prefix) = do
    withQueueRestricted (Set.insert prefix)
    msg $ printf "Restricted command: %s" prefix
handleCommand (Unrestrict prefix) = do
    withQueueRestricted (Set.delete prefix)
    msg $ printf "Unrestricted command: %s" prefix
handleCommand New = do
    withQueueQueue (const Seq.empty)
    msg "Created a new queue!"
handleCommand Open = do
    setQueueOpen True
    msg "The queue is open! Type !enter to enter"
handleCommand Close = do
    setQueueOpen False
    msg "The queue is closed"
handleCommand Start = do
    current <- getCurrentTip
    next <- getNextUp
    msg $ case (current, next) of
        (Just current, Just next) -> printf
            "First match against %s! Next up is %s" (getUserOrTeam current) (getUserOrTeam next)
        (Just current, Nothing) -> printf "First match against %s!" (getUserOrTeam current)
        (Nothing, _) -> "Can't start because the queue is empty!"
handleCommand Win = do
    opponent <- getCurrentTip
    streamerUser <- getQueue queueStreamer
    streamer <- userOrTeamBasedOnMode streamerUser
    case (streamer, opponent) of
        (_, Nothing) -> msg "There's no one in the queue"
        (Nothing, _) -> msg "The streamer is not on a team so I can't mark a win"
        (Just streamer, Just opponent) -> do
            withQueueSetWins (+1)
            endMatch streamer opponent
handleCommand Lose = do
    opponent <- getCurrentTip
    streamerUser <- getQueue queueStreamer
    streamer <- userOrTeamBasedOnMode streamerUser
    case (streamer, opponent) of
        (_, Nothing) -> msg "There's no one in the queue"
        (Nothing, _) -> msg "The streamer is not on a team so I can't mark a loss"
        (Just streamer, Just opponent) -> do
            withQueueSetLosses (+1)
            endMatch opponent streamer
handleCommand Skip = do
    skipped <- getCurrentTip
    withQueueSetWins (const 0)
    withQueueSetLosses (const 0)
    withQueueQueue (Seq.drop 1)
    current <- getCurrentTip
    msg $ case (skipped, current) of
        (Nothing, _) -> "The queue is empty. No one to skip"
        (Just skipped, Nothing) -> printf "Skipped %s. The queue is now empty" (getUserOrTeam skipped)
        (Just skipped, Just current) -> printf "Skipped %s. Next up is %s!"
            (getUserOrTeam skipped)
            (getUserOrTeam current)
handleCommand (Move target rank) = do
    withQueueQueue (Seq.filter (/=target))
    withQueueQueue (insert rank target)
    msg $ printf "Moved %s to position %d in the queue" (getUserOrTeam target) rank
handleCommand (SetCrewStock crew stock) = do
    case crew of
        A -> setQueueCrewStockA stock
        B -> setQueueCrewStockB stock
    msg $ printf "Set crew stock for crew %s to %d" (show crew) stock
handleCommand (FriendList maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    friendMes <- fmap Map.keysSet getFriendMesForMode
    friendMeQueue <- getQueue (Seq.take limit . Seq.filter (\f -> Set.member f friendMes) . queueQueue)
    msg $ printf "Next %d friendmes in the queue: %s" limit (printQueue friendMeQueue)
handleCommand (FriendClear maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    friendMes <- getFriendMesForMode
    friendMeQueue <- getQueue (Seq.take limit . Seq.filter (\f -> Set.member f (Map.keysSet friendMes)) . queueQueue)
    mapM_ (\userOrTeam -> maybe (return ()) (\f -> withQueueFriendMes (Set.delete f)) (Map.lookup userOrTeam friendMes)) friendMeQueue
    msg $ printf "Cleared friend list with limit %d" limit
handleCommand (GetNNID user) = do
    info <- getQueue (Map.lookup user . queueIndex)
    msg $ case info of
        Just (nnid, _, _, _) -> printf "%s's nnid is %s" (getTwitchUser user) (getNNID nnid)
        Nothing -> printf "%s is not in the index. Try !index nnid miiname" (getTwitchUser user)
handleCommand (Index user nnid miid) = do
    currentInfo <- getQueue (Map.lookup user . queueIndex)
    case currentInfo of
        Nothing -> withQueueIndex (Map.insert user (nnid, miid, 0, 0))
        Just (_, _, wins, losses) -> withQueueIndex (Map.insert user (nnid, miid, wins, losses))
    msg $ printf "Added %s to index" (getTwitchUser user)
handleCommand (Friend user) = do
    withQueueFriendMes (Set.insert user)
    msg $ printf "Added %s to friendme list" (getTwitchUser user)
handleCommand (List maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    current <- getCurrentTip
    case current of
        Nothing -> msg "Queue is empty."
        Just current -> do
            queue <- getQueue (Seq.drop 1 . Seq.take limit . queueQueue)
            let msgStart = printf "Currently playing %s." (getUserOrTeam current)
            msg $ case Seq.null queue of
                True -> msgStart ++ " No other entries in the queue."
                False -> msgStart ++ printf " Next in queue: %s" (printQueue queue)
handleCommand (RuleSet maybeMode) = do
    currentMode <- getQueue queueMode
    let mode = fromMaybe currentMode maybeMode
    ruleset <- case mode of
        Singles -> getQueue queueRulesSingles
        Doubles -> getQueue queueRulesDoubles
        Crew -> getQueue queueRulesCrew
        InvalidMode garbageMode -> return $ garbageMode ++ " is not a valid mode. Choose singles, doubles, or cb."
    msg $ printf "Rules for %s: %s"
        (show mode)
        ruleset
handleCommand (Leave user) = do
    userOrTeam <- userOrTeamBasedOnMode user
    case userOrTeam of
        Nothing -> msg $ printf "%s is not on a team, so I can't remove them from the queue." (getTwitchUser user)
        Just userOrTeam -> removeFromQueue userOrTeam
handleCommand (Remove userOrTeam) = removeFromQueue userOrTeam
handleCommand (Info user) = do
    maybeInfo <- getQueue (Map.lookup user . queueIndex)
    userOrTeam <- userOrTeamBasedOnMode user
    position <- case userOrTeam of
        Nothing -> return Nothing
        Just userOrTeam -> getQueue (Seq.findIndexL (==userOrTeam) . queueQueue)
    queueSize <- getQueue (Seq.length . queueQueue)
    msg $ case (maybeInfo, position) of
        (Nothing, _) -> printf "%s is not in the index. Add yourself with !index nnid miiName." (getTwitchUser user)
        (Just (nnid, miiName, wins, losses), Nothing) -> printf "| %s | NNID: %s | MiiName: %s | %d:%d" 
            (getTwitchUser user)
            (getNNID nnid)
            (getMiiName miiName)
            wins
            losses
        (Just (nnid, miiName, wins, losses), Just position) ->
            printf "| %s | Position %d:%d in queue | NNID: %s | MiiName: %s | %d:%d"
                (getTwitchUser user)
                position
                queueSize
                (getNNID nnid)
                (getMiiName miiName)
                wins
                losses
handleCommand (Enter user) = do
    open <- getQueue queueOpen
    indexed <- getQueue (Map.member user . queueIndex)
    maybeUserOrTeam <- userOrTeamBasedOnMode user
    case (open, indexed, maybeUserOrTeam) of
        (False, _, _) -> msg "Sorry the queue is closed so you can't !enter. Use !smash open to open the queue."
        (_, False, _) -> msg $ printf "%s is not in the index. Add yourself with !index nnid miiName." (getTwitchUser user)
        (_, _, Nothing) -> msg $ printf "Couldn't add %s to queue. Try joining a team." (getTwitchUser user)
        (_, _, Just userOrTeam) -> do
            withQueueQueue (Seq.|> userOrTeam)
            position <- getQueue (Seq.length . queueQueue)
            msg $ printf "Added %s to the queue! You are at position %d"
                (getUserOrTeam userOrTeam)
                        position
handleCommand (Here user) = do
    time <- getQueue queueLastMessage
    withQueueHereMap (Map.insert user time)
    msg $ printf "Okay! %s is ready!" (getTwitchUser user)
handleCommand (NewTeam creator team) = do
    existingTeam <- getQueue (Map.lookup creator . queueTeams)
    case existingTeam of
        Just existingTeam -> msg $ printf "Sorry %s, you are already in a team! You can leave your current team (%s) with !teamleave." (getTwitchUser creator) (getTeamName existingTeam)
        Nothing -> do
            withQueueTeams (Map.insert creator team)
            msg $ printf "%s has created the new team %s! Invite an ally with !teaminv [name]." (getTwitchUser creator) (getTeamName team)
handleCommand (TeamInvite inviter invited) = do
    invitingTeam <- getQueue (Map.lookup inviter . queueTeams)
    invitedTeam <- getQueue (Map.lookup invited . queueTeams)
    case (invitingTeam, invitedTeam) of
        (Nothing, _) -> msg $ printf "Sorry %s, you can't invite someone to your team until you create a team with !teamcreate teamname." (getTwitchUser inviter)
        (_, Just invitedTeam) -> msg $ printf "Sorry %s, %s is already on team %s."
            (getTwitchUser inviter)
            (getTwitchUser invited)
            (getTeamName invitedTeam)
        (Just invitingTeam, _) -> do
            withQueueInvites (Map.alter (addToSet invitingTeam) invited)
            msg $ printf "%s, you've been invited join %s. Type !accept %s to accept or !decline %s to decline."
                (getTwitchUser invited)
                (getTeamName invitingTeam)
                (getTeamName invitingTeam)
                (getTeamName invitingTeam)
    where
        addToSet v (Just s) = Just (Set.insert v s)
        addToSet v Nothing = Just (Set.singleton v)
handleCommand (Accept user team) = do
    existingTeam <- getQueue (Map.lookup user . queueTeams)
    teamSize <- getQueue (Map.size . Map.filter (==team) . queueTeams)
    invited <- getQueue (maybe False (Set.member team) . Map.lookup user . queueInvites)
    case (invited, teamSize, existingTeam) of
        (_, 0, _) -> msg $ printf "Sorry %s, but team %s does not exist."
            (getTwitchUser user)
            (getTeamName team)
        (False, _, _) -> msg $ printf "Sorry %s, but you weren't invited to team %s."
            (getTwitchUser user)
            (getTeamName team)
        (_, _, Just existingTeam) -> msg $ printf "Sorry %s, but you're already on team %s. Use !teamleave if you want to leave that team to join %s."
            (getTwitchUser user)
            (getTeamName existingTeam)
            (getTeamName team)
        (_, 2, _) -> msg $ printf "Sorry %s, but team %s is already full."
            (getTwitchUser user)
            (getTeamName team)
        (_, _, Nothing) -> do
            withQueueTeams (Map.insert user team)
            msg $ printf "%s has joined team %s!"
                (getTwitchUser user)
                (getTeamName team)
handleCommand (Decline user team) = do
    wasInvited <- getQueue (maybe False (Set.member team) . Map.lookup user . queueInvites)
    withQueueInvites (Map.adjust (Set.delete team) user)
    msg $ case wasInvited of
        False -> printf "%s, you weren't invited to join team %s."
            (getTwitchUser user)
            (getTeamName team)
        True -> printf "%s declined to join team %s."
            (getTwitchUser user)
            (getTeamName team)
handleCommand (LeaveTeam user) = do
    existingTeam <- getQueue (Map.lookup user . queueTeams)
    case existingTeam of
        Nothing -> msg $ printf "%s, you weren't on a team."
            (getTwitchUser user)
        Just existingTeam -> do
            withQueueTeams (Map.delete user)
            msg $ printf "Removed %s from team %s."
                (getTwitchUser user)
                (getTeamName existingTeam)
handleCommand (Streamer user) = do
    setQueueStreamer user
    msg $ printf "%s is now the streamer!" (getTwitchUser user)
handleCommand (DenyReply reply) = do
    setQueueDenyReply reply
    msg $ printf "%s is the new deny reply." reply
handleCommand (WinBuffer val) = do
    setAndShow "win buffer" setQueueWinBuffer val
handleCommand (ListBuffer val) = do
    setAndShow "list buffer" setQueueListBuffer val
handleCommand (SinglesLimit val) = do
    setAndShow "singles limit" setQueueSinglesLimit val
handleCommand (DoublesLimit val) = do
    setAndShow "doubles limit" setQueueDoublesLimit val
handleCommand (ReenterWait val) = do
    setAndShow "reenter wait" setQueueReenterWait val
handleCommand (HereTimeout val) = do
    setAndShow "here timeout" setQueueHereAlert val
handleCommand (BestOf Singles val) = do
    setQueueSinglesBestOf val
    msg $ printf "Singles best of is %d" val
handleCommand (BestOf Doubles val) = do
    setQueueDoublesBestOf val
    msg $ printf "Doubles best of is %d" val
handleCommand (BestOf mode val) = do
    msg $ printf "Can't set best of for mode %s" (show mode)
handleCommand _ = msg "Not implemented yet!"

setAndShow :: Show a => String -> (Maybe a -> State Queue ()) -> Maybe a -> State Queue (Maybe Message)
setAndShow name set val = do
    set val
    msg $ printf "The %s is %s." name (showMaybeOff val)

showMaybeOff :: Show a => Maybe a -> String
showMaybeOff Nothing = "off"
showMaybeOff (Just v) = show v

removeFromQueue :: UserOrTeam -> State Queue (Maybe Message)
removeFromQueue userOrTeam = do
    queueSize <- getQueue (Seq.length . queueQueue)
    withQueueQueue (Seq.filter (/=userOrTeam))
    queueSize' <- getQueue (Seq.length . queueQueue)
    msg $ case queueSize == queueSize' of
        True -> printf "%s is not in the queue, so I can't remove them from the queue." (getUserOrTeam userOrTeam)
        False -> printf "Removed %s from the queue." (getUserOrTeam userOrTeam)

getQueue :: (Queue -> a) -> State Queue a
getQueue f = fmap f get

printQueue :: Seq.Seq UserOrTeam -> String
printQueue = foldr (\entry result -> result ++ getUserOrTeam entry ++ " ") ""

insert :: Int -> a -> Seq.Seq a -> Seq.Seq a
insert i a s = (\(l, r) -> l Seq.>< (a Seq.<| r)) $ Seq.splitAt i s

endMatch :: UserOrTeam -> UserOrTeam -> State Queue (Maybe Message)
endMatch winner loser = do
    mode <- getQueue queueMode
    case mode of
        Singles -> do
            let winnerUser = TwitchUser . getUserOrTeam $ winner
            let loserUser = TwitchUser . getUserOrTeam $ loser
            winnerInfo <- getQueue (Map.lookup winnerUser . queueIndex)
            case winnerInfo of
                Nothing -> return ()
                Just (nnid, miiname, wins, losses) -> withQueueIndex (Map.insert winnerUser (nnid, miiname, wins + 1, losses))
            loserInfo <- getQueue (Map.lookup loserUser . queueIndex)
            case loserInfo of
                Nothing -> return ()
                Just (nnid, miiname, wins, losses) -> withQueueIndex (Map.insert loserUser (nnid, miiname, wins, losses + 1))
        _ -> return () -- We don't count doubles wins/losses
    setWins <- getQueue queueSetWins
    setLosses <- getQueue queueSetLosses
    required <- getRequiredWins
    let streamerWon = setWins >= required
    let opponentWon = setLosses >= required
    if streamerWon || opponentWon
        then do
            withQueueSetWins (const 0)
            withQueueSetLosses (const 0)
            withQueueQueue (Seq.drop 1)
            current <- getCurrentTip
            let winAndScoreMsg = printf "%s has won the set against %s! The score was %d:%d."
                    (getUserOrTeam winner)
                    (getUserOrTeam loser)
                    setWins
                    setLosses
            msg $ case current of
                Just current -> printf (winAndScoreMsg ++ " Next up is %s")
                    (getUserOrTeam current)
                Nothing -> winAndScoreMsg
        else
            msg $ printf "%s has just won a match against %s! The score is %d:%d and it requires %d to take the set"
                (getUserOrTeam winner)
                (getUserOrTeam loser)
                setWins
                setLosses
                required

msg :: String -> State Queue (Maybe String)
msg m = return (Just m)

printMsg :: String -> State Queue (Maybe String)
printMsg = return . Just . printf

noMsg :: State Queue (Maybe String)
noMsg = return Nothing

simpleQueue f msg q = (f q, Just msg)
messageQueue f msg q = (f q, Just (msg q))
generalizeUser (TwitchUser user) = UserOrTeam user
generalizeTeam (TeamName team) = UserOrTeam team

getCurrentTip :: State Queue (Maybe UserOrTeam)
getCurrentTip = get >>= \q -> case Seq.viewl (queueQueue q) of
    (current Seq.:< _) -> return $ Just current
    _ -> return Nothing

getNextUp :: State Queue (Maybe UserOrTeam)
getNextUp = get >>= \q -> case Seq.viewl (Seq.drop 1 $ queueQueue q) of
    (next Seq.:< _) -> return $ Just next
    _ -> return Nothing

getRequiredWins :: State Queue Int
getRequiredWins = do
    mode <- getQueue queueMode
    bestOf <- case mode of
        Singles -> getQueue queueSinglesBestOf
        Doubles -> getQueue queueDoublesBestOf
        _ -> return 0 -- This doesn't really make sense
    return (ceiling . (/2) . fromIntegral $ bestOf)

getFriendMesForMode :: State Queue (Map.Map UserOrTeam TwitchUser)
getFriendMesForMode = do
    teams <- getQueue queueTeams
    mode <- getQueue queueMode
    friendMeUsers <- getQueue (Set.toList . queueFriendMes)
    return . Map.fromList . catMaybes . fmap (\k -> case getFriendMe mode teams k of
        Nothing -> Nothing
        Just userOrTeam -> Just (userOrTeam, k)) $ friendMeUsers
    where
        getFriendMe mode teams friendMeUser = case mode of
            Singles -> Just . generalizeUser $ friendMeUser
            Doubles -> fmap generalizeTeam . Map.lookup friendMeUser $ teams
            Crew -> Nothing -- TODO: do we need this?
            InvalidMode garbage -> Nothing -- This should never happen

userOrTeamBasedOnMode :: TwitchUser -> State Queue (Maybe UserOrTeam)
userOrTeamBasedOnMode user = get >>= \q -> case queueMode q of
    Doubles -> return $ fmap generalizeTeam $ Map.lookup user (queueTeams q)
    _ -> return $ Just (generalizeUser user)
