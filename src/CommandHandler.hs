module CommandHandler
    ( handleTimestamped
    , getQueueStatus
    ) where

import Command
import CommandParser (twitchUser, commandMap)
import Queue
import Messages

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Tuple
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

handleTimestamped :: TwitchUser -> UTCTime -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleTimestamped user time cmd q = handleMaybeDisabled user cmd (q { queueLastMessage = time })

handleMaybeDisabled :: TwitchUser -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleMaybeDisabled _ (_, On) q = (q { queueOn = True }, Just "qbot enabled. Hello everyone!")
handleMaybeDisabled user cmd q = if queueOn q then handleMaybeRestricted user cmd q else (q, Nothing)

handleMaybeRestricted :: TwitchUser -> (CommandSpec, Command) -> Queue -> (Queue, Maybe Message)
handleMaybeRestricted _ (InvalidCommandSpec, _) q = (q, Just "")
handleMaybeRestricted user (spec, cmd) q =
    if (commandSpecDefaultRestrict spec
        || commandSpecPrefix spec `Set.member` (queueRestricted q))
        && user `Set.notMember` (queueAdmins q)
    then
        (q, Just $ getTwitchUser user ++ " is not allowed to perform command: " ++ commandSpecPrefix spec)
    else swap (runState (handleCommand cmd) q)

handleCommand :: Command -> State Queue (Maybe Message)
handleCommand (Help which) = helpMsg (commandSpecHelp <$> Map.lookup which commandMap, which)
handleCommand (Invalid str) = invalidMsg str
handleCommand Off = do
    setQueueOn False
    disabledMsg
handleCommand (Mode mode) = do
    setQueueMode mode
    modeMsg mode
handleCommand (Allow user) = do
    withQueueAdmins (Set.insert user)
    allowMsg user
handleCommand (Deny user) = do
    withQueueAdmins (Set.delete user)
    denyMsg user
handleCommand (Restrict prefix) = do
    withQueueRestricted (Set.insert prefix)
    restrictMsg prefix
handleCommand (Unrestrict prefix) = do
    withQueueRestricted (Set.delete prefix)
    unrestrictMsg prefix
handleCommand New = do
    withQueueQueue (const Seq.empty)
    newQueueMsg
handleCommand Open = do
    setQueueOpen True
    setQueueSoftClose False
    withQueueSoftClosedList (const Set.empty)
    openMsg
handleCommand Close = do
    setQueueOpen False
    setQueueSoftClose False
    withQueueSoftClosedList (const Set.empty)
    closeMsg
handleCommand SoftClose = do
    setQueueSoftClose True
    withQueueSoftClosedList (const Set.empty)
    softCloseMsg
handleCommand Start = do
    current <- getCurrentTip
    next <- getNextUp
    mode <- getQueue queueMode
    if mode == Singles
        then do
            currentIdentifier <- sequence $ lookupIdentifier . twitchUser . getUserOrTeam <$> current
            nextIdentifier <- sequence $ lookupIdentifier . twitchUser . getUserOrTeam <$> next
            startMsg (currentIdentifier, nextIdentifier)
        else startMsg (current, next)
handleCommand Win = do
    maybeOpponent <- getCurrentTip
    streamerUser <- getQueue queueStreamer
    maybeStreamer <- userOrTeamBasedOnMode streamerUser
    case (maybeStreamer, maybeOpponent) of
        (_, Nothing) -> emptyQueueMsg
        (Nothing, _) -> streamerNotOnATeamWinMsg
        (Just streamer, Just opponent) -> do
            withQueueSetWins (+1)
            endMatch streamer opponent
handleCommand Lose = do
    maybeOpponent <- getCurrentTip
    streamerUser <- getQueue queueStreamer
    maybeStreamer <- userOrTeamBasedOnMode streamerUser
    case (maybeStreamer, maybeOpponent) of
        (_, Nothing) -> emptyQueueMsg
        (Nothing, _) -> streamerNotOnATeamLossMsg
        (Just streamer, Just opponent) -> do
            withQueueSetLosses (+1)
            endMatch opponent streamer
handleCommand Skip = do
    maybeSkipped <- getCurrentTip
    withQueueSetWins (const 0)
    withQueueSetLosses (const 0)
    withQueueQueue (Seq.drop 1)
    maybeCurrent <- getCurrentTip
    skipMsg (maybeSkipped, maybeCurrent)
handleCommand (Move target rank) = do
    withQueueQueue (Seq.filter (/=target))
    withQueueQueue (insert rank target)
    moveMsg (target, rank)
handleCommand (SetCrewStock crew stock) = do
    case crew of
        A -> setQueueCrewStockA stock
        B -> setQueueCrewStockB stock
    setCrewStockMsg (crew, stock)
handleCommand (FriendList maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    friendMes <- fmap Map.keysSet getFriendMesForMode
    friendMeQueue <- getQueue (Seq.take limit . Seq.filter (\f -> Set.member f friendMes) . queueQueue)
    friendListMsg (limit, friendMeQueue)
handleCommand (FriendClear maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    friendMes <- getFriendMesForMode
    friendMeQueue <- getQueue (Seq.take limit . Seq.filter (\f -> Set.member f (Map.keysSet friendMes)) . queueQueue)
    mapM_ (\userOrTeam -> maybe
        (return ())
        (\f -> withQueueFriendMes (appEndo . foldMap Endo . fmap Set.delete $ f))
        (Map.lookup userOrTeam friendMes)) friendMeQueue
    friendListClearMsg limit
handleCommand (GetSmashTag user) = do
    info <- getQueue (Map.lookup user . queueIndex)
    getSmashTagMsg (user, info)
handleCommand (Index user smashTag dolphinVersion) =
    case dolphinVersion of
        InvalidDolphin badDolphinVersion -> badDolphinMsg (user, badDolphinVersion)
        _ -> do
            currentInfo <- getQueue (Map.lookup user . queueIndex)
            let newInfo = getNewInfo currentInfo
            withQueueIndex (Map.insert user newInfo)
            indexMsg (user, newInfo)
    where
        getNewInfo maybeCurrentInfo = 
            case maybeCurrentInfo of
                Nothing -> IndexEntry smashTag dolphinVersion 0 0
                Just currentInfo -> currentInfo { indexedSmashTag = smashTag, indexedDolphinVersion = dolphinVersion}
handleCommand (Friend user) = do
    withQueueFriendMes (Set.insert user)
    friendMeMsg user
handleCommand (List maybeLimit) = do
    let limit = fromMaybe 10 maybeLimit
    maybeCurrent <- getCurrentTip
    remainder <- getQueue (Seq.drop 1 . Seq.take limit . queueQueue)
    listMsg (maybeCurrent, remainder)
handleCommand (RuleSet maybeMode) = do
    currentMode <- getQueue queueMode
    let mode = fromMaybe currentMode maybeMode
    ruleset <- case mode of
        Singles -> getQueue queueRulesSingles
        Doubles -> getQueue queueRulesDoubles
        Crew -> getQueue queueRulesCrew
        InvalidMode garbageMode -> return $ garbageMode ++ " is not a valid mode. Choose singles, doubles, or cb."
    ruleSetMsg (mode, ruleset)
handleCommand Stagelist = do
    stagelist <- getQueue queueStagelist
    stageListMsg stagelist
handleCommand (Leave user) = do
    maybeUserOrTeam <- userOrTeamBasedOnMode user
    case maybeUserOrTeam of
        Nothing -> unableToRemoveMsg user
        Just userOrTeam -> removeFromQueue userOrTeam
handleCommand (Remove userOrTeam) = removeFromQueue userOrTeam
handleCommand (Info user) = do
    maybeInfo <- getQueue (Map.lookup user . queueIndex)
    maybeUserOrTeam <- userOrTeamBasedOnMode user
    maybePosition <- case maybeUserOrTeam of
        Nothing -> return Nothing
        Just userOrTeam -> getQueue (fmap (+1) . Seq.findIndexL (==userOrTeam) . queueQueue)
    queueSize <- getQueue (Seq.length . queueQueue)
    infoMsg (user, queueSize, maybeInfo, maybePosition)
handleCommand (Enter user) = do
    open <- getQueue queueOpen
    maybeUserInfo <- getQueue (Map.lookup user . queueIndex)
    maybeUserOrTeam <- userOrTeamBasedOnMode user
    userOrTeamInSoftCloseList <- getQueue (Set.member maybeUserOrTeam . Set.map Just . queueSoftClosedList)
    queueIsSoftClosed <- getQueue queueSoftClose
    let userOrTeamSoftClosed = queueIsSoftClosed && userOrTeamInSoftCloseList
    queue <- getQueue queueQueue
    let alreadyInQueue = maybe False (isJust . flip Seq.elemIndexL queue) maybeUserOrTeam
    position <- case (open, maybeUserInfo, maybeUserOrTeam, alreadyInQueue, userOrTeamSoftClosed) of
        (True, Just _, Just userOrTeam, False, False) -> do
            withQueueQueue (Seq.|> userOrTeam)
            when queueIsSoftClosed (withQueueSoftClosedList (Set.insert userOrTeam))
            getQueue (Seq.length . queueQueue)
        (_, _, _, _, _) -> return 0
    mode <- getQueue queueMode
    if not open
        then msg "Sorry the queue is closed so you can't !enter. An admin must use !smash open to open the queue."
        else case maybeUserInfo of
            Nothing -> user & " is not in the index. Add yourself with !index smashTag dolphinVerson."
            Just userInfo -> case maybeUserOrTeam of
                Nothing -> "Couldn't add " & user % " to queue. Try joining a team."
                Just userOrTeam -> do
                    if alreadyInQueue
                        then "Sorry " & userOrTeam % ", you can't join the queue more than once!"
                        else if userOrTeamSoftClosed then "Sorry " & userOrTeam % ", you can't join the queue again because it is soft closed!"
                        else if mode == Singles then userIdentifier user userInfo & ", you've now been placed into the queue at position " % position % "! Type !info to see your position."
                        else userOrTeam & ", you've now been placed into the queue at position " % position % "! Type !info to see your position."
handleCommand (Here user) = do
    time <- getQueue queueLastMessage
    withQueueHereMap (Map.insert user time)
    hereMsg user
handleCommand (NewTeam creator team) = do
    maybeExistingTeam <- getQueue (Map.lookup creator . queueTeams)
    case maybeExistingTeam of
        Just existingTeam -> alreadyOnTeamMsg (creator, existingTeam)
        Nothing -> do
            withQueueTeams (Map.insert creator team)
            newTeamMsg (creator, team)
handleCommand (TeamInvite inviter invited) = do
    maybeInvitingTeam <- getQueue (Map.lookup inviter . queueTeams)
    maybeInvitedTeam <- getQueue (Map.lookup invited . queueTeams)
    case (maybeInvitingTeam, maybeInvitedTeam) of
        (Nothing, _) -> "Sorry " & inviter % ", you can't invite someone to your team until you create a team with !teamcreate teamname."
        (_, Just invitedTeam) -> "Sorry " & inviter % ", "
            % invited % " is already on team " % invitedTeam
        (Just invitingTeam, _) -> do
            withQueueInvites (Map.alter (addToSet invitingTeam) invited)
            invited & ", you've been invited join " % invitingTeam
                % ". Type !accept " % invitingTeam
                % " to accept or !decline " % invitingTeam % " to decline."
    where
        addToSet v (Just s) = Just (Set.insert v s)
        addToSet v Nothing = Just (Set.singleton v)
handleCommand (Accept user givenTeam) = do
    openInvites <- getQueue (maybe [] Set.toList . Map.lookup user . queueInvites)
    let singleInviteTeam = case openInvites of { (onlyInvite:[]) -> Just onlyInvite; _ -> Nothing; }
    let maybeTeam = listToMaybe (catMaybes [givenTeam, singleInviteTeam])
    case (maybeTeam, openInvites) of
        (Just team, _) -> do
            invited <- getQueue (maybe False (Set.member team) . Map.lookup user . queueInvites)
            teamSize <- getQueue (Map.size . Map.filter (==team) . queueTeams)
            maybeExistingTeam <- getQueue (Map.lookup user . queueTeams)
            case (invited, teamSize, maybeExistingTeam) of
                (_, 0, _) -> "Sorry " & user % ", but team " % team % " does not exist."
                (False, _, _) -> "Sorry " & user % ", but you weren't invited to team " % team % "."
                (_, _, Just existingTeam) -> "Sorry " & user
                    % ", but you're already on team " % existingTeam
                    % ". Use !teamleave if you want to leave that team to join "
                    % team % "."
                (_, 2, _) -> "Sorry " & user
                    % ", but team " % team % " is already full."
                (_, _, Nothing) -> do
                    withQueueTeams (Map.insert user team)
                    user & " has joined team " % team % "!"
        (Nothing, []) -> "Sorry " & user % ". You don't have any team invites."
        (Nothing, invites) -> "Sorry " & user
            % ". You have multiple team invites: " % invites
            % ". Please specify which one you want to accept."
handleCommand (Decline user team) = do
    wasInvited <- getQueue (maybe False (Set.member team) . Map.lookup user . queueInvites)
    withQueueInvites (Map.adjust (Set.delete team) user)
    case wasInvited of
        False -> user & ", you weren't invited to join team " % team % "."
        True -> user & " declined to join team " % team % "."
handleCommand (TeamInfo team) = do
    teamMembers <- getQueue (Map.keys . Map.filter (==team) . queueTeams)
    case teamMembers of
        [] -> "Team " & team % " does not exist. Create it with !teamcreate."
        (member:[]) -> handleCommand (Info member)
        (member1:member2:_) -> do
            info1 <- getQueue (Map.lookup member1 . queueIndex)
            info2 <- getQueue (Map.lookup member2 . queueIndex)
            friendme1 <- getQueue (Set.member member1 . queueFriendMes)
            friendme2 <- getQueue (Set.member member2 . queueFriendMes)
            let friendmeSuffix1 = if friendme1 then "+" else ""
            let friendmeSuffix2 = if friendme2 then "+" else ""
            case (info1, info2) of
                (Nothing, _) -> "Team member " & member1 % " is no longer in the index!"
                (_, Nothing) -> "Team member " & member2 % " is no longer in the index!"
                (Just indexEntry1, Just indexEntry2) ->
                    "| " & team
                    % " | " % member1 % friendmeSuffix1
                    % " & " % member2 % friendmeSuffix2
                    % " | " % indexedSmashTag indexEntry1 % " & " % indexedSmashTag indexEntry2
                    % " |"
handleCommand (LeaveTeam user) = do
    maybeExistingTeam <- getQueue (Map.lookup user . queueTeams)
    case maybeExistingTeam of
        Nothing -> user & ", you weren't on a team."
        Just existingTeam -> do
            withQueueTeams (Map.delete user)
            "Removed " & user % " from team " % existingTeam % "."
handleCommand (Streamer user) = do
    setQueueStreamer user
    user & " is now the streamer!"
handleCommand (DenyReply reply) = do
    setQueueDenyReply reply
    reply & " is the new deny reply."
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
    "Singles best of is " & val
handleCommand (BestOf Doubles val) = do
    setQueueDoublesBestOf val
    "Doubles best of is " & val
handleCommand (BestOf mode _) = do
    "Can't set best of for mode " & mode
handleCommand _ = msg "Not implemented yet!"

lookupIdentifier :: TwitchUser -> State Queue UserIdentifier
lookupIdentifier user = do
    maybeInfo <- Map.lookup user <$> getQueue queueIndex
    return $ case maybeInfo of
        Just info -> userIdentifier user info
        _ -> UserIdentifier user (SmashTag "unknown tag")

setAndShow :: Show a => String -> (Maybe a -> State Queue ()) -> Maybe a -> State Queue (Maybe Message)
setAndShow name set val = do
    set val
    "The " & name % " is " % (showMaybeOff val) % "."

showMaybeOff :: Show a => Maybe a -> String
showMaybeOff Nothing = "off"
showMaybeOff (Just v) = show v

removeFromQueue :: UserOrTeam -> State Queue (Maybe Message)
removeFromQueue userOrTeam = do
    queueSize <- getQueue (Seq.length . queueQueue)
    withQueueQueue (Seq.filter (/=userOrTeam))
    queueSize' <- getQueue (Seq.length . queueQueue)
    case queueSize == queueSize' of
        True -> notInTheQueueMsg userOrTeam
        False -> removedMsg userOrTeam

insert :: Int -> a -> Seq.Seq a -> Seq.Seq a
insert i a s = (\(l, r) -> l Seq.>< (a Seq.<| r)) $ Seq.splitAt i s

endMatch :: UserOrTeam -> UserOrTeam -> State Queue (Maybe Message)
endMatch winner loser = do
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
            mode <- getQueue queueMode
            case mode of
                Singles -> do
                    let winnerUser = twitchUser . getUserOrTeam $ winner
                    let loserUser = twitchUser . getUserOrTeam $ loser
                    winnerInfo <- getQueue (Map.lookup winnerUser . queueIndex)
                    case winnerInfo of
                        Nothing -> return ()
                        Just winnerEntry ->
                            withQueueIndex (Map.insert winnerUser (winnerEntry { indexedWins = indexedWins winnerEntry + 1 }))
                    loserInfo <- getQueue (Map.lookup loserUser . queueIndex)
                    case loserInfo of
                        Nothing -> return ()
                        Just loserEntry ->
                            withQueueIndex (Map.insert loserUser (loserEntry { indexedLosses = indexedLosses loserEntry + 1 }))
                _ -> return () -- We don't count doubles wins/losses
            current <- getCurrentTip
            setWinMsg (winner, loser, setWins, setLosses, current)
        else
            winMsg (winner, loser, setWins, setLosses, required)

getQueueStatus :: Queue -> String
getQueueStatus = fromMaybe "" . evalState (do
    maybeCurrent <- getCurrentTip
    maybeNext <- getNextUp
    case (maybeCurrent, maybeNext) of
        (Just current, Just next) -> "Currently Playing: " & current % " - Up Next: " % next % " -"
        (Just current, Nothing) -> "Currently Playing: " & current % " - There is no one else in the queue -"
        (_, _) -> msg "The queue is currently empty -"
    )
