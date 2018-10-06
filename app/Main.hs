{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Maybe
import Control.Applicative  ((<$>))
import System.Environment   (getEnv)
import Control.Monad.Except (MonadIO, MonadError, catchError, liftIO)
import Data.Text (Text, pack, unpack)

import Data

import System.Environment

import Database.Bolt

defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4jneo4j", host = "localhost", port = 7687}

-- docker run --publish=7474:7474 --publish=7687:7687 neo4j:3.0
-- cmd
    -- set GRAPHENEDB_BOLT_HOST=localhost
    -- set GRAPHENEDB_BOLT_PORT=7687
    -- set GRAPHENEDB_BOLT_USER=neo4j
    -- set GRAPHENEDB_BOLT_PASSWORD=neo4j
     
main :: IO ()
main = run `catchError` failMsg
  where run = do config <- readConfig `catchError` const (return defaultConfig)
                 putStrLn $ "Enter command"
                 line <- getLine
                 let (command : argList) = parseLine ' ' line
                 pipe <- connect config
                 runCommand pipe command argList
                 close pipe
        readConfig = do
            host <- read <$> getEnv "GRAPHENEDB_BOLT_HOST"
            port <- read <$> getEnv "GRAPHENEDB_BOLT_PORT"
            user <- read <$> getEnv "GRAPHENEDB_BOLT_USER"
            pass <- read <$> getEnv "GRAPHENEDB_BOLT_PASSWORD"
            return def { user = user, password = pass, host = host, port = port }

 
runCommand :: Pipe -> String -> [String] -> IO()
runCommand pipe command argList
    | command == "exit" = putStrLn "Bye!"
    | command == "e"    = putStrLn "Bye!"
    | command == "createDemo" = do
            run pipe $ createDemo
            main
    | command == "getReactionById" = do
        if argList == [] 
        then putStrLn $ "For this request you need to enter the id of reaction"
        else do
            maybeReaction <- run pipe (getReactionById (read $ head argList) )
            case maybeReaction of 
                Nothing         -> putStrLn $ "Reaction with id " ++ (head argList) ++ " not found"
                Just reaction   -> putStrLn $ show reaction
            main    
    | command == "getShortestPath" = do
        case argList of
            (start:end:_) -> do
                path <- run pipe $ getShortestPath start end
                if path == [] 
                then putStrLn $ "Path not found" 
                else putStrLn $ intercalate " -> " (zipWith (\x i -> (if odd i then "Molecule" else "Reaction") ++ " {id = " ++ show x ++ "}") path [1..])

            otherwise     -> putStrLn $ "For this request you need to enter the SMILES of initial molecule and target molecule"
        main
    | otherwise = doesntExist command argList

doesntExist :: String -> [String] -> IO()
doesntExist command _ =
    if null command
        then main
        else do
            putStrLn $ "Command '" ++ command ++ "' doesn't exist"
            main

failMsg :: (MonadError e m, MonadIO m, Show e) => e -> m ()
failMsg e = liftIO $ putStrLn ("Ooops: " ++ show e)

parseLine :: Char -> String -> [String]
parseLine delimiter s = foldr f [[]] s where
    f x rest@(r:rs)
        | x == delimiter  = [] : rest
        | otherwise = (x : r) : rs
