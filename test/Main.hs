{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Database.Bolt
import Control.Monad.Except (liftIO)

import Data

config :: BoltCfg
config = def {user = "neo4j", password = "neo4j"}

main :: IO Counts
main = runTestTT $ TestList [testCase1, testCase2, testCase3, testCase4, testCase5]
    
test1 :: BoltActionT IO Bool
test1 = do
        Just id <- createReaction "test" [] [] [] 
        Just reaction <- getReactionById id
        return (id == (idReaction reaction) )

test2 :: BoltActionT IO Bool
test2 = do
        Just id <- createReaction "test" [] [] [] 
        Just reaction <- getReactionById id
        return ( "Reaction {id=" ++ show id ++ ", name='test', reagents=[], catalysts= [], products= []}" == (show reaction) )

test3 :: BoltActionT IO Bool
test3 = do
        maybeReaction <- getReactionById (-1)
        return ( maybeReaction == Nothing )

test4 :: BoltActionT IO Bool
test4 = do
        path <- getShortestPath "O=O" "O"
        return ( (length path) == 3 )

test5 :: BoltActionT IO Bool
test5 = do
        path <- getShortestPath "O" "O=O"
        return ( path == [] )

testCase1 = TestCase (do pipe <- connect config
                         result <- run pipe test1
                         assertBool "createReaction and getReactionById test #1" result)

testCase2 = TestCase (do pipe <- connect config
                         result <- run pipe test2
                         assertBool "createReaction and getReactionById test #2" result)

testCase3 = TestCase (do pipe <- connect config
                         result <- run pipe test3
                         assertBool "getReactionById test" result)

testCase4 = TestCase (do pipe <- connect config
                         run pipe createDemo
                         result <- run pipe test4
                         assertBool "createDemo and getShortestPath test #1" result)

testCase5 = TestCase (do pipe <- connect config
                         run pipe createDemo
                         result <- run pipe test5
                         assertBool "createDemo and getShortestPath test #2" result)



