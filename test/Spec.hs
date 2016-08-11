import Queue hiding (test)
import Command

import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid
import Data.Time
import System.Exit

tests =
    [ test "Can't enter closed queue" $ do
        user "a" "!enter"
        botSay "Sorry the queue is closed so you can't !enter. Use !smash open to open the queue."
    , test "Can't enter non-indexed user" $ do
        streamer "!smash open"
        user "a" "!enter"
        botSay "a is not in the index. Add yourself with !index NNID MiiName."
    , test "Can enter indexed users into open queue" $ do
        streamer "!smash open"
        indexAndEnter "a"
        botSay "Added a to the queue! You are at position 1"
    , test "Non-enqueued info output formatted properly" $ do
        user "xio" "!index xionnid xiomii"
        user "xio" "!info"
        botSay "| User: xio | NNID: xionnid | MiiName: xiomii | W:L 0:0 |"
    , test "Enqueued info output formatted properly" $ do
        streamer "!smash open"
        user "xio" "!index xionnid xiomii"
        user "xio" "!enter"
        user "xio" "!info"
        botSay "| User: xio | Position 1/1 in Queue | NNID: xionnid | MiiName: xiomii | W:L 0:0 |"
    , test "Absent !info output formatted properly" $ do
        user "xio" "!info"
        botSay "xio is not in the index. Add yourself with !index NNID MiiName."
    , test "Known !nnid formatted properly" $ do
        user "xio" "!index xionnid xiomii"
        user "xio" "!nnid"
        botSay "xio's NNID is xionnid."
    , test "Unknown !nnid formatted properly" $ do
        user "xio" "!nnid"
        botSay "xio is not in the index. Try !index NNID MiiName."
    , test "Start formatted properly" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        streamer "!smash start"
        botSay "First match against xio (xiomiiname)!"
    , test "Start formatted properly (two in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash start"
        botSay "First match against xio (xiomiiname)! Next up is zio (ziomiiname)!"
    , test "Start formatted properly (empty queue)" $ do
        streamer "!smash open"
        streamer "!smash start"
        botSay "Can't start because the queue is empty!"
    , test "Win formatted properly (streamer takes set; two in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash start"
        streamer "!win"
        streamer "!lose"
        streamer "!win"
        botSay "josuf107 has won the set against xio! The score was 2:1. Next up is zio (ziomiiname)!"
    , test "Skip formatted properly (two in queue)" $ do
        streamer "!smash open"
        indexAndEnter "xio"
        indexAndEnter "zio"
        streamer "!smash start"
        streamer "!smash skip"
        botSay "Skipped xio. Next up is zio (ziomiiname)!"
    ]

indexAndEnter u = do
    user u ("!index " ++ u ++ "nnid " ++ u ++ "miiname")
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
user u = tellOne . UserSay (TwitchUser u)
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
