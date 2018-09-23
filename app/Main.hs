{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Maybe
import Control.Applicative  ((<$>))
import System.Environment   (getEnv)

import Data

import System.Environment

import Database.Bolt

defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4jneo4j"}

-- docker run --publish=7474:7474 --publish=7687:7687 neo4j:3.0

main :: IO ()
main = do
    putStrLn $ "Enter command"
    line <- getLine
    let (command : argList) = parseLine ' ' line
    pipe <- connect defaultConfig
    runCommand pipe command argList
    close pipe

 
runCommand :: Pipe -> String -> [String] -> IO()
runCommand pipe command argList
    | command == "exit" = putStrLn "Bye!"
    | command == "e"    = putStrLn "Bye!"
    | command == "createDemo" = do
            run pipe $ createDemo
            main
            | command == "createReaction" = do
{-        if argList == [] 
        then putStrLn "For this request you need to enter the name of reaction, id's of reagents and products"
        else do
            let name = head argList
            let rIdList = parseLine '>' $ argList !! 0
            let caIdList = parseLine '>' $ argList !! 1
            let pIdList = parseLine '>' $ argList !! 2
            
            putStrLn $ show rIdList
            putStrLn $ show caIdList
            putStrLn $ show pIdList
            mr <- createReaction 
                "dehydrogenation methane" 
                [(Molecule 0 "C" "methane", 2)]
                [(Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"))]
                [Accelerate 1400 100] 
                [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)] 

            case mr of 
                Nothing         -> putStrLn $ "Error: could not insert the reaction"
                Just reaction   -> putStrLn $ show reaction
-}
            main
    | command == "getReactionById" = do
        if argList == [] 
        then putStrLn $ "For this request you need to enter the id of reaction"
        else do
            maybeReaction <- run pipe (getReactionById (read $ head argList) )
            case maybeReaction of 
                Nothing         -> putStrLn $ "Reaction with id " ++ (head argList) ++ " was not found"
                Just reaction   -> putStrLn $ show reaction
            main    
    | otherwise = doesntExist command argList

doesntExist :: String -> [String] -> IO()
doesntExist command _ =
    if null command
        then main
        else do
            putStrLn $ "Command '" ++ command ++ "' doesn't exist"
            main


parseLine :: Char -> String -> [String]
parseLine delimiter s = foldr f [[]] s where
    f x rest@(r:rs)
        | x == delimiter  = [] : rest
        | otherwise = (x : r) : rs

