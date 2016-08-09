-- This is the state data structure and the implementation of the commands on
-- it
module Queue where

import Command

import Control.Monad
import Control.Monad.State
import Data.Tuple
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
    , queueMode :: Mode
    , queueAdmins :: Set.Set TwitchUser
    , queueRestricted :: Set.Set String
    , queueQueue :: Seq.Seq UserOrTeam
    , queueOpen :: Bool
    , queueTeams :: Map.Map TwitchUser TeamName
    , queueStreamer :: TwitchUser
    , queueSetWins :: Int
    , queueSetLosses :: Int
    , queueBestOf :: Int
    , queueCrewStockA :: Int
    , queueCrewStockB :: Int
    , queueFriendMes :: Set.Set TwitchUser
    , queueIndex :: Map.Map TwitchUser (NNID, MiiName)
    } deriving (Show, Eq, Ord)
type Message = String

defaultQueue = Queue
    { queueOn = True
    , queueMode = Singles
    , queueAdmins = Set.fromList [TwitchUser "josuf107"]
    , queueRestricted = Set.empty
    , queueQueue = Seq.empty
    , queueOpen = False
    , queueTeams = Map.empty
    , queueStreamer = TwitchUser "josuf107"
    , queueSetWins = 0
    , queueSetLosses = 0
    , queueBestOf = 3
    , queueCrewStockA = 3
    , queueCrewStockB = 3
    , queueFriendMes = Set.empty
    , queueIndex = Map.empty
    }

type Modify a = a -> a

setQueueOn :: Bool -> State Queue ()
setQueueOn v = modify $ \q -> q { queueOn = v }
setQueueMode :: Mode -> State Queue ()
setQueueMode v = modify $ \q -> q { queueMode = v }
withQueueAdmins :: Modify (Set.Set TwitchUser) -> State Queue ()
withQueueAdmins f = modify $ \q -> q { queueAdmins = f (queueAdmins q) }
withQueueRestricted :: Modify (Set.Set String) -> State Queue ()
withQueueRestricted f = modify $ \q -> q { queueRestricted = f (queueRestricted q) }
withQueueQueue :: Modify (Seq.Seq UserOrTeam) -> State Queue ()
withQueueQueue f = modify $ \q -> q { queueQueue = f (queueQueue q) }
setQueueOpen :: Bool -> State Queue ()
setQueueOpen v = modify $ \q -> q { queueOpen = v }
withQueueTeams :: Modify (Map.Map TwitchUser TeamName) -> State Queue ()
withQueueTeams f = modify $ \q -> q { queueTeams = f (queueTeams q) }
withQueueSetWins :: Modify Int -> State Queue ()
withQueueSetWins f = modify $ \q -> q { queueSetWins = f (queueSetWins q) }
withQueueSetLosses :: Modify Int -> State Queue ()
withQueueSetLosses f = modify $ \q -> q { queueSetLosses = f (queueSetLosses q) }
setQueueCrewStockA :: Int -> State Queue ()
setQueueCrewStockA v = modify $ \q -> q { queueCrewStockA = v }
setQueueCrewStockB :: Int -> State Queue ()
setQueueCrewStockB v = modify $ \q -> q { queueCrewStockB = v }
withQueueFriendMes :: Modify (Set.Set TwitchUser) -> State Queue ()
withQueueFriendMes f = modify $ \q -> q { queueFriendMes = f (queueFriendMes q) }
withQueueIndex :: Modify (Map.Map TwitchUser (NNID, MiiName)) -> State Queue ()
withQueueIndex f = modify $ \q -> q { queueIndex = f (queueIndex q) }

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
    streamerUser <- fmap queueStreamer get
    streamer <- userOrTeamBasedOnMode streamerUser
    case (streamer, opponent) of
        (_, Nothing) -> msg "There's no one in the queue"
        (Nothing, _) -> msg "The streamer is not on a team so I can't mark a win"
        (Just streamer, Just opponent) -> do
            withQueueSetWins (+1)
            endMatch streamer opponent
handleCommand Lose = do
    opponent <- getCurrentTip
    streamerUser <- fmap queueStreamer get
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
    friendMeQueue <- fmap (Seq.take limit . Seq.filter (\f -> Set.member f friendMes) . queueQueue) get
    msg $ printf "Next %d friendmes in the queue: %s" limit (printQueue friendMeQueue)
    where
        getFriendMes mode friendMeUsers teams = case mode of
            Singles -> Set.map (Just . generalizeUser) friendMeUsers
            Doubles -> Set.map (\user -> fmap generalizeTeam . Map.lookup user $ teams) friendMeUsers
            Crew ->  Set.empty -- TODO: do we need this?
handleCommand (FriendClear maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    friendMes <- getFriendMesForMode
    friendMeQueue <- fmap (Seq.take limit . Seq.filter (\f -> Set.member f (Map.keysSet friendMes)) . queueQueue) get
    mapM_ (\userOrTeam -> maybe (return ()) (\f -> withQueueFriendMes (Set.delete f)) (Map.lookup userOrTeam friendMes)) friendMeQueue
    msg $ printf "Cleared friend list with limit %d" limit
handleCommand (GetNNID user) = do
    index <- fmap queueIndex get
    msg $ case Map.lookup user index of
        Just (nnid, _) -> printf "%s's nnid is %s" (getTwitchUser user) (getNNID nnid)
        Nothing -> printf "%s is not in the index. Try !index nnid miiname" (getTwitchUser user)
handleCommand (Index user nnid miid) = do
    withQueueIndex (Map.insert user (nnid, miid))
    msg $ printf "Added %s to index" (getTwitchUser user)
handleCommand (Friend user) = do
    withQueueFriendMes (Set.insert user)
    msg $ printf "Added %s to friendme list" (getTwitchUser user)
handleCommand (Enter user) = do
    open <- fmap queueOpen get
    if not open
        then msg "Sorry the queue is closed so you can't !enter. Use !smash open to open the queue"
        else do
            maybeUserOrTeam <- userOrTeamBasedOnMode user
            case maybeUserOrTeam of
                Just userOrTeam -> do
                    withQueueQueue (Seq.|> userOrTeam)
                    position <- fmap (Seq.length . queueQueue) get
                    msg $ printf "Added %s to the queue! You are at position %d"
                        (getUserOrTeam userOrTeam)
                        position
                Nothing -> msg $ printf "Couldn't add %s to queue. Try joining a team." (getTwitchUser user)
handleCommand (List maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    current <- getCurrentTip
    case current of
        Nothing -> msg "Queue is empty."
        Just current -> do
            queue <- fmap (Seq.drop 1 . Seq.take limit . queueQueue) get
            let msgStart = printf "Currently playing %s." (getUserOrTeam current)
            msg $ case Seq.null queue of
                True -> msgStart ++ " No other entries in the queue."
                False -> msgStart ++ printf " Next in queue: %s" (printQueue queue)
handleCommand _ = msg "Not implemented yet!"

printQueue :: Seq.Seq UserOrTeam -> String
printQueue = foldr (\entry result -> result ++ getUserOrTeam entry ++ " ") ""

insert :: Int -> a -> Seq.Seq a -> Seq.Seq a
insert i a s = (\(l, r) -> l Seq.>< (a Seq.<| r)) $ Seq.splitAt i s

endMatch :: UserOrTeam -> UserOrTeam -> State Queue (Maybe Message)
endMatch winner loser = do
    setWins <- fmap queueSetWins get
    setLosses <- fmap queueSetLosses get
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
getRequiredWins = fmap (ceiling . (/2) . fromIntegral . queueBestOf) get

getFriendMesForMode :: State Queue (Map.Map UserOrTeam TwitchUser)
getFriendMesForMode = do
    teams <- fmap queueTeams get
    mode <- fmap queueMode get
    friendMeUsers <- fmap (Set.toList . queueFriendMes) get
    return . Map.fromList . catMaybes . fmap (\k -> case getFriendMe mode teams k of
        Nothing -> Nothing
        Just userOrTeam -> Just (userOrTeam, k)) $ friendMeUsers
    where
        getFriendMe mode teams friendMeUser = case mode of
            Singles -> Just . generalizeUser $ friendMeUser
            Doubles -> fmap generalizeTeam . Map.lookup friendMeUser $ teams
            Crew -> Nothing -- TODO: do we need this?

userOrTeamBasedOnMode :: TwitchUser -> State Queue (Maybe UserOrTeam)
userOrTeamBasedOnMode user = get >>= \q -> case queueMode q of
    Doubles -> return $ fmap generalizeTeam $ Map.lookup user (queueTeams q)
    _ -> return $ Just (generalizeUser user)
