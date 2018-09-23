{-# LANGUAGE OverloadedStrings #-}

module Data (createReaction, getReactionById, createDemo) where 

import Types

import Data.Char
import Data.Map.Strict (fromList, Map)
import Data.Text (Text, pack, unpack)

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe (fromJust)

import Database.Bolt


createReaction :: String -> [(Molecule, Double)] -> [Catalyst] -> Maybe Accelerate -> [(Molecule, Double)] -> BoltActionT IO (Maybe Reaction)
createReaction name reagentList catalistList accelerate productList = do            
    records <- queryP cypher params
    if null records 
    then pure Nothing
    else do
        result <- toNode (head records)
        pure $ Just (Reaction (nodeIdentity result) name reagentList catalistList accelerate productList) 
    where
        cypher = cypherText name reagentList catalistList accelerate productList
        params = cypherParams name reagentList catalistList accelerate productList      

        cypherText :: String -> [(Molecule, Double)] -> [Catalyst] -> Maybe Accelerate -> [(Molecule, Double)] -> Text
        cypherText name reagentList catalistList accelerate productList = pack $ 
          "MERGE (r:Reaction {name: {name}})" <> 
          cypherHeadReagent <> 
          cypherHeadCatalist <> 
          cypherHeadProduct <> 
          cypherTailReagent <> 
          cypherTailCatalyst <> 
          cypherTailProduct where
            cypherHeadReagent = concat $ zipWith f (map show [1..]) reagentList where
                f i (Molecule _ s u, _) = "MERGE (mr" <> i <> ":Molecule {" <> 
                                          "name:{name_r" <> i <> "}," <>
                                          "smiles:{smiles_r" <> i <> "}," <>
                                          "iupacName:{iupacName_r" <> i <> "}})"
                                        
            cypherHeadProduct = concat $ zipWith f (map show [1..]) productList where
                f i (Molecule _ s u, _) = "MERGE (mp" <> i <> ":Molecule {" <> 
                                          "name:{name_p" <> i <> "}," <>
                                          "smiles:{smiles_p" <> i <> "}," <>
                                          "iupacName:{iupacName_p" <> i <> "}})"
                                
            cypherHeadCatalist = case (catalistList, accelerate) of
              ([], Nothing) -> ""
              ([], _      ) -> "MERGE (noCatalist:Catalyst {smiles:'',name:'no catalist'})"
              otherwise     -> concat $ zipWith f (map show [1..]) catalistList where
                f i (Catalyst _ s (Just n)) = "MERGE (c" <> i <> ":Catalyst {" <> 
                                              "smiles:{smiles_c" <> i <> "}," <>
                                              "name:{name_c" <> i <> "}})"
                f i (Catalyst _ s  _      ) = "MERGE (c" <> i <> ":Catalyst {" <> 
                                              "smiles:{smiles_c" <> i <> "}})"
            
            cypherTailReagent = concat $ zipWith f (map show [1..]) reagentList where
                f i m = "MERGE (mr" <> i <> ")-[:REAGENT_IN {amount: {amount_r" <> i <> "}}]->(r)"

            cypherTailProduct = concat $ zipWith f (map show [1..]) productList where
                f i m = "MERGE (r)-[:PRODUCT_FROM {amount: {amount_p" <> i <> "}}]->(mp" <> i <>")"
    
            cypherTailCatalyst = case (catalistList, accelerate) of
              ([], Nothing) -> ""
              ([], _      ) -> "MERGE (noCatalist)-[:ACCELERATE {temperature:{t},pressure:{p}}]->(r)"
              (_ , Nothing) -> concat $ zipWith f (map show [1..]) catalistList where
                f i c = "MERGE (c" <> i <> ")-[:ACCELERATE]->(r)"
              otherwise     -> concat $ zipWith f (map show [1..]) catalistList where
                f i c = "MERGE (c" <> i <> ")-[:ACCELERATE {temperature:{t},pressure:{p}}]->(r)"
                    
        cypherParams :: String -> [(Molecule, Double)] -> [Catalyst] -> Maybe Accelerate -> [(Molecule, Double)] -> Map Text Value
        cypherParams name reagentList catalistList accelerate productList = fromList $ 
          [("name", T $ pack name)] <> 
          paramListReagent <> 
          paramListProduct <> 
          paramListCatalyst <> 
          paramListAccelerate where        
            
            paramListReagent = concat $ zipWith f (map show [1..]) reagentList where
                f :: String -> (Molecule, Double) -> [(Text, Value)]
                f i (Molecule _ s u, a) = [(pack $ "name_r"      <> i, T $ pack u), 
                                           (pack $ "smiles_r"    <> i, T $ pack s),
                                           (pack $ "iupacName_r" <> i, T $ pack u),
                                           (pack $ "amount_r"    <> i, F a)]
                                           
            paramListProduct = concat $ zipWith f (map show [1..]) productList where
                f :: String -> (Molecule, Double) -> [(Text, Value)]
                f i (Molecule _ s u, a) = [(pack $ "name_p"      <> i, T $ pack u), 
                                           (pack $ "smiles_p"    <> i, T $ pack s),
                                           (pack $ "iupacName_p" <> i, T $ pack u),
                                           (pack $ "amount_p"    <> i, F a)]
            
            paramListCatalyst = concat $ zipWith f (map show [1..]) catalistList where
                f :: String -> Catalyst -> [(Text, Value)]
                f i (Catalyst _ s (Just n)) = [(pack $ "smiles_c"  <> i, T $ pack s), 
                                               (pack $ "name_c"    <> i, T $ pack n)]
                f i (Catalyst _ s _       ) = [(pack $ "smiles_c"  <> i, T $ pack s)]
            
            paramListAccelerate 
                | accelerate == Nothing = []
                | otherwise             = [("t", F $ temperature (fromJust accelerate)), ("p", F $ pressure (fromJust accelerate))]
                


getReactionById :: Int -> BoltActionT IO (Maybe Reaction)
getReactionById id = do 
    records <- queryP cypher params
    if null records 
    then pure Nothing
    else do
        result <- toNode (head records)
        let props = nodeProps result
        let identity = nodeIdentity result
        name <- (props `at` "name") >>= exact
        pure $ Just (Reaction id (unpack name) [] [] Nothing [])

    where 
        cypher = "match (reagent)-[amount:REAGENT_IN]-(reaction:Reaction) WHERE id(reaction)=id return reaction, collect ([reagent, amount])"
        
        cypher = "MATCH (reaction) WHERE id(reaction)={id}" <>
                 "MATCH (reaction)<-[amount_in:REAGENT_IN]-(reagent:Molecule) " <>
                 "MATCH (reaction)-[amount_from:REAGENT_FROM]->(product:Molecule) " <>
                 "OPTIONAL MATCH (reaction)-[accelerate:ACCELERATE]->(catalyst:Catalyst) " <>
                 
                 "RETURN reaction.name as name," <>
                 "collect([reagent,amount_in]) as reagents," <>
                 "collect([product,amount_from]) as products," <>
                 "collect([accelerate,catalyst]) as catalysts," <>

        
        
        params = fromList [("id", I id)]

--    match (r:Reaction), (a)-[c:REAGENT_IN]-(r), (a)-[p:PRODUCT_FROM]-(r), (a)-[p:PRODUCT_FROM]-(r)  WHERE id(r)=201 return r, a, c"
-- match (reagent)-[amount:REAGENT_IN]-(reaction:Reaction) WHERE id(reaction)=201 return reaction, collect ([reagent, amount])        

createDemo :: BoltActionT IO ()
createDemo = do
        query "MATCH (n) DETACH DELETE n"
        
        createReaction 
            "dehydrogenation methane" 
            [(Molecule 0 "C" "methane", 2)]
            [(Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"))]
            (Just (Accelerate 1400 100))
            [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)]
    
        createReaction 
            "thermal cracking of propane" 
            [(Molecule 0 "CCC" "propane", 1)]
            []
            (Just (Accelerate 500 0))
            [(Molecule 0 "C=CC" "prop-1-ene", 1), (Molecule 0 "[H]" "molecular hydrogen", 1)] 
                        
        createReaction 
            "hydration of ethanol" 
            [(Molecule 0 "O" "oxidane", 1), (Molecule 0 "C=C" "ethene", 1)]
            [(Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"))]
            (Just (Accelerate 300 7))
            [(Molecule 0 "CCO" "ethanol", 1)]
            
        createReaction 
            "combustion of ethanol" 
            [(Molecule 0 "CCO" "ethanol", 1), (Molecule 0 "O=O" "molecular oxygen", 3)]
            []
            Nothing 
            [(Molecule 0 "C(=O)=O" "carbon dioxide", 2), (Molecule 0 "O" "oxidane", 1)]

        createReaction 
            "butadiene synthesis" 
            [(Molecule 0 "CCO" "ethanol", 2)]
            [(Catalyst 0 "O=[Zn]" (Just "oxozinc")), Catalyst 0 "[O-2].[O-2].[O-2].[Al+3].[Al+3]" (Just "aluminum oxide") ]
            (Just (Accelerate 300 0))
            [(Molecule 0 "C=CC=C" "buta-1,3-diene", 2.0), (Molecule 0 "O" "oxidane", 2), (Molecule 0 "[H]" "molecular hydrogen", 1)]
        
        createReaction 
            "sodium methoxide synthesis" 
            [(Molecule 0 "[Na]" "sodium", 2), (Molecule 0 "CO" "methanol", 2)]
            []
            Nothing 
            [(Molecule 0 "[H]" "molecular hydrogen", 1), (Molecule 0 "[Na+].[O-]C" "sodium methoxide", 2)]
        
        createReaction 
            "dehydrogenation methane" 
            [(Molecule 0 "C" "methane", 2)]
            [(Catalyst 0 "O=[Zn]" (Just "oxozinc"))]
            (Just (Accelerate 1400 0))
            [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)]

        createReaction 
            "trimerization of acetylene" 
            [(Molecule 0 "C#C" "acetylene", 3)]
            [(Catalyst 0 "[C]" (Just "carbon"))]
            (Just (Accelerate 650 0))
            [(Molecule 0 "c1ccccc1" "benzene ", 1)]

        pure ()

--    match (s:Molecule), (t:Molecule), path=shortestPath((s)-[*]-(t)) WHERE id(s)=168 and id(t)=157 return path
--    match (s:Molecule), (t:Molecule), path=shortestPath((s)-[*]-(t)) WHERE s.smiles="O=O" and t.smiles="C=CC=C" return path