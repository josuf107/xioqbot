-- This is the state data structure and the implementation of the commands on
-- it
module Queue where

import Command

import Control.Monad.State
import Data.Tuple
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

data Queue
    = Queue
    { queueOn :: Bool
    , queueMode :: Mode
    , queueAdmins :: Set.Set TwitchUser
    , queueRestricted :: Set.Set String
    , queueQueue :: Seq.Seq UserOrTeam
    , queueOpen :: Bool
    , queueTeams :: Map.Map TwitchUser TeamName
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
    }

setQueueOn v q = q { queueOn = v }
setQueueMode v q = q { queueMode = v }
withQueueAdmins f q = q { queueAdmins = f (queueAdmins q) }
withQueueRestricted f q = q { queueRestricted = f (queueRestricted q) }
withQueueQueue f q = q { queueQueue = f (queueQueue q) }
setQueueOpen v q = q { queueOpen = v }
withQueueTeams f q = q { queueTeams = f (queueTeams q) }

handleMaybeDisabled :: TwitchUser -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleMaybeDisabled user (_, On) q = (setQueueOn True q, Just "qbot enabled. Hello everyone!")
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
    modify (setQueueOn False)
    return . Just $ "qbot disabled. !smash on to re-enable"
handleCommand (Mode mode) = do
    modify (setQueueMode mode)
    return . Just $ "New mode is: " ++ show mode
handleCommand (Allow user) = do
    modify (withQueueAdmins (Set.insert user))
    return . Just $ "Added admin: " ++ getTwitchUser user
handleCommand (Deny user) = do
    modify (withQueueAdmins (Set.delete user))
    return . Just $ "Removed admin: " ++ getTwitchUser user
handleCommand (Restrict prefix) = do
    modify (withQueueRestricted (Set.insert prefix))
    return . Just $ "Restricted command: " ++ prefix
handleCommand (Unrestrict prefix) = do
    modify (withQueueRestricted (Set.delete prefix))
    return . Just $ "Unrestricted command: " ++ prefix
handleCommand New = do
    modify (withQueueQueue (const Seq.empty))
    return . Just $ "Created a new queue!"
handleCommand Open = do
    modify (setQueueOpen True)
    return . Just $ "The queue is open! Type !enter to enter"
handleCommand Close = do
    modify (setQueueOpen False)
    return . Just $ "The queue is closed"
handleCommand Start = get >>=
    \q -> return . Just $ case (currentTip q, nextUp q) of
        (Just current, Just next) -> "First match against " ++ getUserOrTeam current ++ "! Next up is " ++ getUserOrTeam next
        (Just current, Nothing) -> "First match against " ++ getUserOrTeam current ++ "!"
        (Nothing, _) -> "Can't start because the queue is empty!"
handleCommand (Enter user) = get >>=
    \q -> case userOrTeamBasedOnMode q user of
        Just userOrTeam -> do
            modify (withQueueQueue (Seq.|> userOrTeam))
            return . Just $ "Added " ++ getUserOrTeam userOrTeam ++ " to the queue! You are at position " ++ (show . (+1) . Seq.length . queueQueue $ q)
        Nothing -> return . Just $ "Couldn't add " ++ getTwitchUser user ++ " to queue. Try joining a team."
handleCommand _ = return . Just $ "Not implemented yet!"

simpleQueue f msg q = (f q, Just msg)
messageQueue f msg q = (f q, Just (msg q))
generalizeUser (TwitchUser user) = UserOrTeam user
generalizeTeam (TeamName team) = UserOrTeam team

currentTip :: Queue -> Maybe UserOrTeam
currentTip q = case Seq.viewl (queueQueue q) of
    (current Seq.:< _) -> Just current
    _ -> Nothing

nextUp :: Queue -> Maybe UserOrTeam
nextUp q = case Seq.viewl (Seq.drop 1 $ queueQueue q) of
    (next Seq.:< _) -> Just next
    _ -> Nothing

userOrTeamBasedOnMode :: Queue -> TwitchUser -> Maybe UserOrTeam
userOrTeamBasedOnMode q user = case queueMode q of
    Doubles -> fmap generalizeTeam $ Map.lookup user (queueTeams q)
    _ -> Just (generalizeUser user)
