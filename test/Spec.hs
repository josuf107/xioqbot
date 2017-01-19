import Queue hiding (test)
import Command

import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid
import Data.Time
import System.Exit

tests =
    [ test "Off" $ do
        streamer "!smash off"
        botSay "qbot disabled. !smash on to re-enable"
    , test "Off ignores commands" $ do
        streamer "!smash off"
        streamer "!info"
        botSayNothing
    , test "On" $ do
        streamer "!smash on"
        botSay "qbot enabled. Hello everyone!"
    , test "On turns on" $ do
        streamer "!smash off"
        streamer "!smash on"
        botSay "qbot enabled. Hello everyone!"
    , test "Mode singles" $ do
        streamer "!smash mode singles"
        botSay "New mode is: Singles"
    , test "Mode doubles" $ do
        streamer "!smash mode doubles"
        botSay "New mode is: Doubles"
    , test "Mode crew" $ do
        streamer "!smash mode cb"
        botSay "New mode is: Crew"
    , test "Allow" $ do
        streamer "!smash allow xio"
        botSay "Added admin: Xio"
    , test "Allow allows" $ do
        streamer "!smash allow xio"
        xio "!smash off"
        botSay "qbot disabled. !smash on to re-enable"
    , test "Deny" $ do
        streamer "!smash deny xio"
        botSay "Removed admin: Xio"
    , test "Deny denies" $ do
        streamer "!smash allow xio"
        streamer "!smash deny xio"
        xio "!smash off"
        botSay "Xio is not allowed to perform command: smash off"
    , test "Restrict" $ do
        streamer "!smash restrict enter"
        botSay "Restricted command: enter"
    , test "Restrict restricts" $ do
        streamer "!smash restrict enter"
        xio "!enter"
        botSay "Xio is not allowed to perform command: enter"
    , test "Unrestrict" $ do
        streamer "!smash unrestrict enter"
        botSay "Unrestricted command: enter"
    , test "Unrestrict unrestricts" $ do
        streamer "!smash restrict enter"
        streamer "!smash unrestrict enter"
        xio "!enter"
        botSay "Sorry the queue is closed so you can't !enter. An admin must use !smash open to open the queue."
    , test "New" $ do
        streamer "!smash new"
        botSay "Created a new queue!"
    , test "New clears queue" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        streamer "!smash new"
        streamer "!list"
        botSay "Queue is empty."
    , test "Open" $ do
        streamer "!smash open"
        botSay "The queue is open! Type !enter to enter"
    , test "Open opens the queue" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        botSay "Xio, you've now been placed into the queue at position 1! Type !info to see your position and !friendme if you've yet to add Josuf107."
    , test "Close" $ do
        streamer "!smash close"
        botSay "The queue is closed"
    , test "Close closes" $ do
        streamer "!smash open"
        streamer "!smash close"
        indexAndEnter "xio"
        botSay "Sorry the queue is closed so you can't !enter. An admin must use !smash open to open the queue."
    , test "Start (singles; one in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        streamer "!smash start"
        botSay "The first match is beginning and the opponent is Xio (xiomiiname)!"
    , test "Start (singles; two in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash start"
        botSay "The first match is beginning and the opponent is Xio (xiomiiname)! Next up is Zio (ziomiiname)!"
    , test "Start (singles; empty queue)" $ do
        streamer "!smash open"
        streamer "!smash start"
        botSay "Can't start because the queue is empty!"
    , test "Start (doubles; one in queue)" $ do
        streamer "!smash mode doubles"
        streamer "!smash open"
        index "xio"
        index "zio"
        user "xio" "!teamcreate xioteam"
        user "xio" "!teaminv zio"
        user "zio" "!accept xioteam"
        user "xio" "!enter"
        streamer "!smash start"
        botSay "The first match is beginning and the opponent is Xioteam (xiomiiname & ziomiiname)!"
    , test "Start (doubles; two in queue)" $ do
        streamer "!smash mode doubles"
        streamer "!smash open"
        index "xio"
        index "zio"
        user "xio" "!teamcreate xioteam"
        user "xio" "!teaminv zio"
        user "zio" "!accept xioteam"
        user "xio" "!enter"
        index "foo"
        index "bar"
        user "foo" "!teamcreate footeam"
        user "foo" "!teaminv bar"
        user "bar" "!accept footeam"
        user "foo" "!enter"
        streamer "!smash start"
        botSay "The first match is beginning and the opponent is Xioteam (xiomiiname & ziomiiname)! Next up is Footeam (barmiiname & foomiiname)!"
    , test "Start (doubles; empty queue)" $ do
        streamer "!smash open"
        streamer "!smash start"
        botSay "Can't start because the queue is empty!"
    , test "Win" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        streamer "!smash start"
        streamer "!win"
        botSay "Josuf107 has just won a match against Xio! The score is 1:0 and it requires 2 to take the set"
    , test "Win (streamer takes set; two in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash start"
        streamer "!win"
        streamer "!lose"
        streamer "!win"
        botSay "Josuf107 has won the set against Xio! The score was 2:1. Next up is Zio (ziomiiname)!"
    , test "Win (streamer takes set; one in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        streamer "!smash start"
        streamer "!win"
        streamer "!lose"
        streamer "!win"
        botSay "Josuf107 has won the set against Xio! The score was 2:1."
    , test "Win (doubles; streamer takes set; two in queue)" $ do
        streamer "!smash mode doubles"
        streamer "!smash open"
        index "bro"
        streamer "!teamcreate steamrollers"
        streamer "!teaminv bro"
        user "bro" "!accept steamrollers"
        index "xio"
        index "zio"
        xio "!teamcreate xioteam"
        xio "!teaminv zio"
        zio "!accept xioteam"
        xio "!enter"
        index "a"
        index "b"
        user "a" "!teamcreate a-team"
        user "a" "!teaminv b"
        user "b" "!accept a-team"
        user "a" "!enter"
        streamer "!smash start"
        streamer "!win"
        streamer "!lose"
        streamer "!win"
        botSay "Steamrollers has won the set against Xioteam! The score was 2:1. Next up is A-team (amiiname & bmiiname)!"
    , test "Skip (two in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash start"
        streamer "!smash skip"
        botSay "Skipped Xio. Next up is Zio (ziomiiname)!"
    , test "Skip (doubles; two in queue)" $ do
        streamer "!smash mode doubles"
        streamer "!smash open"
        index "xio"
        index "zio"
        xio "!teamcreate xfactor"
        xio "!teaminv zio"
        zio "!accept xfactor"
        xio "!enter"
        index "a"
        index "b"
        user "a" "!teamcreate a-team"
        user "a" "!teaminv b"
        user "b" "!accept a-team"
        user "a" "!enter"
        streamer "!smash start"
        streamer "!smash skip"
        botSay "Skipped Xfactor. Next up is A-team (amiiname & bmiiname)!"
    , test "Move" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash move zio 0"
        streamer "!list"
        botSay "Currently playing Zio. Next in queue: Xio (xiomiiname)"
    , test "Can't enter closed queue" $ do
        user "a" "!enter"
        botSay "Sorry the queue is closed so you can't !enter. An admin must use !smash open to open the queue."
    , test "Can't enter non-indexed user" $ do
        streamer "!smash open"
        user "a" "!enter"
        botSay "A is not in the index. Add yourself with !index NNID MiiName."
    , test "Can enter indexed users into open queue" $ do
        streamer "!smash open"
        indexAndEnter "a"
        botSay "A, you've now been placed into the queue at position 1! Type !info to see your position and !friendme if you've yet to add Josuf107."
    , test "Non-enqueued info output formatted properly" $ do
        user "xio" "!index xionnid xiomii"
        user "xio" "!info"
        botSay "| User: Xio | NNID: xionnid | MiiName: xiomii | W:L 0:0 |"
    , test "Enqueued info output formatted properly" $ do
        streamer "!smash open"
        user "xio" "!index xionnid xiomii"
        user "xio" "!enter"
        user "xio" "!info"
        botSay "| User: Xio | Position 1/1 in Queue | NNID: xionnid | MiiName: xiomii | W:L 0:0 |"
    , test "Absent !info output formatted properly" $ do
        user "xio" "!info"
        botSay "Xio is not in the index. Add yourself with !index NNID MiiName."
    , test "Known !nnid formatted properly" $ do
        user "xio" "!index xionnid xiomii"
        user "xio" "!nnid"
        botSay "Xio's NNID is xionnid."
    , test "Unknown !nnid formatted properly" $ do
        user "xio" "!nnid"
        botSay "Xio is not in the index. Try !index NNID MiiName."
    , test "Accept" $ do
        index "xio"
        index "zio"
        xio "!teamcreate xfactor"
        xio "!teaminv zio"
        zio "!accept xfactor"
        botSay "Zio has joined team Xfactor!"
    , test "Accept (default team)" $ do
        index "xio"
        index "zio"
        xio "!teamcreate xfactor"
        xio "!teaminv zio"
        zio "!accept"
        botSay "Zio has joined team Xfactor!"
    , test "Accept (several possible defaults)" $ do
        index "xio"
        index "zio"
        index "mo"
        xio "!teamcreate xfactor"
        xio "!teaminv zio"
        user "mo" "!teamcreate mobetta"
        user "mo" "!teaminv zio"
        zio "!accept"
        botSay "Sorry Zio. You have multiple team invites: Mobetta, Xfactor. Please specify which one you want to accept."
    , test "Accept (no default team)" $ do
        index "zio"
        zio "!accept"
        botSay "Sorry Zio. You don't have any team invites."
    , test "SoftClose" $ do
        streamer "!smash open"
        streamer "!smash softclose"
        indexAndEnter "xio"
        botSay "Xio, you've now been placed into the queue at position 1! Type !info to see your position and !friendme if you've yet to add Josuf107."
        streamer "!smash skip"
        xio "!enter"
        botSay "Sorry Xio, you can't join the queue again because it is soft closed!"
    ]

xio = user "xio"
zio = user "zio"
index u = do
    user u ("!index " ++ u ++ "nnid " ++ u ++ "miiname")
indexAndEnter u = do
    index u
    user u "!enter"

main :: IO ()
main = do
    putStrLn "\nRunning test suite..."
    results <- mapM runTest tests
    when (any (/=Pass) results) exitFailure

test desc spec = Test spec desc
wait :: Seconds -> TestSpec
wait = tellOne . Wait
streamer :: String -> TestSpec
streamer = tellOne . StreamerSay
user :: String -> String -> TestSpec
user u = tellOne . UserSay (twitchUser u)
botSay :: String -> TestSpec
botSay = tellOne . BotSay . Just
botSayNothing :: TestSpec
botSayNothing = tellOne (BotSay Nothing)
tellOne = tell . return

data TestStep
    = Wait Seconds
    | StreamerSay String
    | UserSay TwitchUser String
    | BotSay (Maybe String)
    deriving (Show, Eq, Ord)

type TestSpec = Writer [TestStep] ()

data Test = Test
    { testSpec :: TestSpec
    , testDescription :: String
    } deriving (Show, Eq, Ord)

data TestResult = Pass | Fail String deriving (Show, Eq, Ord)

data TestState = TestState
    { testStateTime :: UTCTime
    , testStateQueue :: Queue
    , testStateLastMessage :: Maybe String
    } deriving (Show, Eq, Ord)

runTest :: Test -> IO TestResult
runTest t = do
    let result = runTestSpec (testSpec t)
    putStrLn $ case result of
        Pass -> testDescription t ++ ": Passed!"
        Fail s -> testDescription t ++ ": Failed! " ++ s
    return result

runTestSpec :: TestSpec -> TestResult
runTestSpec spec = runSteps . snd . runWriter $ spec

runSteps :: [TestStep] -> TestResult
runSteps steps =
    case dropWhile (==Pass) . fst . runState (mapM runStep steps) $ testStateStart of
        [] -> Pass
        (f:_) -> f

testStateStart = TestState
    (UTCTime (fromGregorian 2016 8 10) 0)
    defaultQueue
    Nothing

runStep :: TestStep -> State TestState TestResult
runStep (Wait n) = do
    modify (\ts -> ts { testStateTime = addUTCTime (fromIntegral n) (testStateTime ts) })
    return Pass
runStep (StreamerSay s) = do
    u <- fmap (queueStreamer . testStateQueue) get
    runStep (UserSay u s)
runStep (UserSay u s) = do
    time <- fmap testStateTime get
    q <- fmap testStateQueue get
    let (q', msg) = handleTimestamped u time (parseCommand u s) q
    modify (\ts -> ts { testStateQueue = q', testStateLastMessage = msg })
    return Pass
runStep (BotSay s) = do
    lastMsg <- fmap testStateLastMessage get
    if lastMsg == s then return Pass else return (Fail $ "Expected " ++ show s ++ " but was " ++ show lastMsg)
