module Main where

import Lib

import Data.Maybe
import System.Environment

main :: IO ()
main = do
    env <- getEnvironment
    let botUser = lookupDefault "xioqbot" "BOT_USER" env
    let botToken = lookupDefault "Need a token" "BOT_TOKEN" env
    let botChannel = lookupDefault "#josuf107" "BOT_CHANNEL" env
    talk botUser botToken botChannel

lookupDefault :: Eq a => b -> a -> [(a, b)] -> b
lookupDefault def key source = fromMaybe def (lookup key source)
