{-# LANGUAGE OverloadedStrings #-}

module Data
    ( ServerState (..)
    , constructState
    , queryMoleculeById
    ) where

import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           Data.Map.Strict            (fromList, (!))
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool, createPool)
import           Data.Text                  (Text)
import           Database.Bolt

import           Type

-- |A pool of connections to Neo4j server
data ServerState = ServerState { pool :: Pool Pipe }


-- |Returns molecule by id
queryMoleculeById :: Text -> BoltActionT IO Molecule
queryMoleculeById id = do result <- head <$> queryP cypher params
                      T id <- result `at` "id"
                      T smiles <- result `at` "smiles"
                      T iupacName <- result `at` "iupacName"
                      return $ Molecule id smiles iupacName

  where cypher = "MATCH (molecule:Molecule {id:{id}}) " <>
                 "RETURN molecule.id as id," <>
                 "molecule.smiles as smiles," <>
                 "molecule.iupacName as iupacName" <>
                 "LIMIT 1"
        params = fromList [("id", T id)]


-- |Create pool of connections (4 stripes, 500 ms timeout, 1 resource per stripe)
constructState :: BoltCfg -> IO ServerState
constructState bcfg = do pool <- createPool (connect bcfg) close 4 500 1
                         return (ServerState pool)
