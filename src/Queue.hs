-- This is the state data structure
module Queue where

import Command
import CommandParser (twitchUser)

import Control.Monad.State
import Data.Maybe
import Data.Time
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Queue
    = Queue
    { queueOn :: Bool
    , queueLastMessage :: UTCTime
    , queueMode :: Mode
    , queueAdmins :: Set.Set TwitchUser
    , queueRestricted :: Set.Set String
    , queueQueue :: Seq.Seq UserOrTeam
    , queueSoftClosedList :: Set.Set UserOrTeam
    , queueSoftClose :: Bool
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
    , queueStagelist :: String
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

defaultQueue :: Queue
defaultQueue = Queue
    { queueOn = True
    , queueLastMessage = UTCTime (fromGregorian 2016 1 1) 0
    , queueMode = Singles
    , queueAdmins = Set.fromList [twitchUser "josuf107"]
    , queueRestricted = Set.empty
    , queueQueue = Seq.empty
    , queueOpen = False
    , queueSoftClosedList = Set.empty
    , queueSoftClose = False
    , queueTeams = Map.empty
    , queueStreamer = twitchUser "josuf107"
    , queueSetWins = 0
    , queueSetLosses = 0
    , queueCrewStockA = 3
    , queueCrewStockB = 3
    , queueFriendMes = Set.empty
    , queueIndex = Map.empty
    , queueRulesSingles = "2 Stock, 6m Time, No Items, Legal Stages Only | Refer to !stagelist for legal stages."
    , queueRulesDoubles = "3 Stock, 8m Time, No Items, Team Attack On, Legal Stages Only | Refer to !stagelist for legal stages."
    , queueRulesCrew = "It's a crew battle."
    , queueStagelist = "Battlefield, FD(Omega Palutena’s), Smashville, Town and City, Duck Hunt, Dreamland, and Lylat."
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
setQueueLastMessage :: QueueSet UTCTime
setQueueLastMessage v = modify $ \q -> q { queueLastMessage = v }
setQueueMode :: QueueSet Mode
setQueueMode v = modify $ \q -> q { queueMode = v }
withQueueAdmins :: QueueModify (Set.Set TwitchUser)
withQueueAdmins f = modify $ \q -> q { queueAdmins = f (queueAdmins q) }
withQueueRestricted :: QueueModify (Set.Set String)
withQueueRestricted f = modify $ \q -> q { queueRestricted = f (queueRestricted q) }
withQueueQueue :: QueueModify (Seq.Seq UserOrTeam)
withQueueQueue f = modify $ \q -> q { queueQueue = f (queueQueue q) }
withQueueSoftClosedList :: QueueModify (Set.Set UserOrTeam)
withQueueSoftClosedList f = modify $ \q -> q { queueSoftClosedList = f (queueSoftClosedList q) }
setQueueSoftClose :: QueueSet Bool
setQueueSoftClose v = modify $ \q -> q { queueSoftClose = v }
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
setQueueStagelist :: QueueSet String
setQueueStagelist v = modify $ \q -> q { queueStagelist = v }
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

getQueue :: (Queue -> a) -> State Queue a
getQueue f = fmap f get

getFriendMesForMode :: State Queue (Map.Map UserOrTeam [TwitchUser])
getFriendMesForMode = do
    teams <- getQueue queueTeams
    mode <- getQueue queueMode
    friendMeUsers <- getQueue (Set.toList . queueFriendMes)
    return . Map.fromListWith (++) . catMaybes . fmap (\k -> case getFriendMe mode teams k of
        Nothing -> Nothing
        Just userOrTeam -> Just (userOrTeam, [k])) $ friendMeUsers
    where
        getFriendMe mode teams friendMeUser = case mode of
            Singles -> Just . generalizeUser $ friendMeUser
            Doubles -> fmap generalizeTeam . Map.lookup friendMeUser $ teams
            Crew -> Nothing -- TODO: do we need this?
            InvalidMode _ -> Nothing -- This should never happen

generalizeUser :: TwitchUser -> UserOrTeam
generalizeUser (TwitchUser user) = UserOrTeam user

generalizeTeam :: TeamName -> UserOrTeam
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
    return (ceiling . (/(2::Double)) . fromIntegral $ bestOf)

getMiiNames :: UserOrTeam -> State Queue [MiiName]
getMiiNames userOrTeam = do
    mode <- getQueue queueMode
    case mode of
        Singles -> do
            let user = twitchUser . getUserOrTeam $ userOrTeam
            lookupMii user
        Doubles -> do
            let team = TeamName . getUserOrTeam $ userOrTeam
            users <- getQueue (Map.keys . Map.filter (==team) . queueTeams)
            fmap concat (mapM lookupMii users)
        Crew -> return [] -- TODO: Crew
        InvalidMode _ -> return [] -- Shrug
    where
        extractMiiName (_, mii, _, _) = mii
        lookupMii user = getQueue (fmap extractMiiName . maybeToList . Map.lookup user . queueIndex)

userOrTeamBasedOnMode :: TwitchUser -> State Queue (Maybe UserOrTeam)
userOrTeamBasedOnMode user = get >>= \q -> case queueMode q of
    Doubles -> return $ fmap generalizeTeam $ Map.lookup user (queueTeams q)
    _ -> return $ Just (generalizeUser user)
