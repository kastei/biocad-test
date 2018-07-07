module Lib
    ( runner ) where

import Types

runner :: IO ()
runner = do
    command <- getLine
    if null command then runner else exec command

exec "molecule" = do
    putStrLn $ show (Molecule 1 "smiles" "iupacName")
    
exec otherwise = do 
    putStrLn "error"
    runner
