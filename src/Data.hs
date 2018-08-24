{-# LANGUAGE OverloadedStrings #-}

module Data (createReaction, getReactionById, createDemo, createDemo1, test) where 

import Types

import Data.Char
import Data.Map.Strict (fromList, Map)
import Data.Text (Text, pack, unpack)

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe (fromMaybe)

import Database.Bolt


createReaction :: String -> [(Molecule, Double)] -> [Catalyst] -> [Accelerate] -> [(Molecule, Double)] -> BoltActionT IO (Maybe Reaction)
createReaction name reagentList catalistList accelerateList productList = do            
    records <- queryP cypher params
    if null records 
        then pure Nothing
        else do
            result <- toNode (head records)
            pure $ Just (Reaction (nodeIdentity result) name) 
    where
        cypher = getCypherText name reagentList catalistList accelerateList productList
        params = getCypherParams name reagentList catalistList accelerateList productList      
                                                                    
getCypherText :: String -> [(Molecule, Double)] -> [Catalyst] -> [Accelerate] -> [(Molecule, Double)] -> Text
getCypherText name reagentList catalistList accelerateList productList = pack $ "MERGE (r:Reaction {name: {name}})" <> 
    cypherHeadReagent <> cypherHeadCatalist <> cypherHeadProduct <> cypherTailReagent <> cypherTailCatalyst <> cypherTailProduct where

            cypherHeadReagent = concat $ zipWith f [1..] reagentList where
                f i (Molecule _ s u, _) = "MERGE (mr" <> (show i) <> ":Molecule {" <> 
                                          "name:{iupacName_r" <> (show i) <> "}," <>
                                          "smiles:{smiles_r" <> (show i) <> "}," <>
                                          "iupacName:{iupacName_r" <> (show i) <> "}})"
                                        
            cypherHeadProduct = concat $ zipWith f [1..] productList where
                f i (Molecule _ s u, _) = "MERGE (mp" <> (show i) <> ":Molecule {" <> 
                                          "name:{name_p" <> (show i) <> "}," <>
                                          "smiles:{smiles_p" <> (show i) <> "}," <>
                                          "iupacName:{iupacName_p" <> (show i) <> "}})"
                                
            cypherHeadCatalist = if catalistList == []
                then "MERGE (noCatalist:Catalyst {smiles:'',name: 'no catalist'})" 
                else concat $ zipWith f [1..] catalistList where
                    f i (Catalyst _ s (Just n)) = "MERGE (c" <> (show i) <> ":Catalyst {" <> 
                                                  "smiles:{smiles_c" <> (show i) <> "}," <>
                                                  "name:{name_c" <> (show i) <> "}})"
                    f i (Catalyst _ s _       ) = "MERGE (c" <> (show i) <> ":Catalyst {" <> 
                                                  "smiles:{smiles_c" <> (show i) <> "}})"
            
            cypherTailReagent = concat $ zipWith f [1..] reagentList where
                f i m = "MERGE (mr" <> (show i) <> ")-[:REAGENT_IN {amount: {amount_r" <> (show i) <> "}}]->(r)"

            cypherTailProduct = concat $ zipWith f [1..] productList where
                f i m = "MERGE (r)-[:PRODUCT_FROM {amount: {amount_p" <> (show i) <> "}}]->(mp" <> (show i) <>")"
    
            cypherTailCatalyst = if catalistList == [] 
                    then if accelerateList == [] 
                        then ""
                        else "MERGE (noCatalist)-[:ACCELERATE {temperature:{t},pressure:{p}}]->(r)"
                    else concat $ zipWith f [1..] catalistList where
                        f i c = "MERGE (c" <> (show i) <> ")-[:ACCELERATE " <> (if accelerateList == [] then "" else "{temperature:{t},pressure:{p}}") <> "]->(r)"
                
                    
getCypherParams :: String -> [(Molecule, Double)] -> [Catalyst] -> [Accelerate] -> [(Molecule, Double)] -> Map Text Value
getCypherParams name reagentList catalistList accelerateList productList = fromList $ [("name", T $ pack name)] <> paramListReagent <> paramListProduct <> paramListCatalyst <> paramListAccelerate where        
--            paramListReagent :: [(String, Value)]
            paramListReagent = concat $ zipWith f [1..] reagentList where
                f :: Int -> (Molecule, Double) -> [(Text, Value)]
                f i (Molecule _ s u, a) = [(pack $ "name_r"      <> (show i), T $ pack u), 
                                           (pack $ "smiles_r"    <> (show i), T $ pack s),
                                           (pack $ "iupacName_r" <> (show i), T $ pack u),
                                           (pack $ "amount_r"    <> (show i), F a)]
                                           
--            paramListProduct :: [(String, Value)]
            paramListProduct = concat $ zipWith f [1..] productList where
                f :: Int -> (Molecule, Double) -> [(Text, Value)]
                f i (Molecule _ s u, a) = [(pack $ "name_p"      <> (show i), T $ pack u), 
                                           (pack $ "smiles_p"    <> (show i), T $ pack s),
                                           (pack $ "iupacName_p" <> (show i), T $ pack u),
                                           (pack $ "amount_p"    <> (show i), F a)]
            
--            paramListCatalyst :: [(String, Value)]
            paramListCatalyst = concat $ zipWith f [1..] catalistList where
                f :: Int -> Catalyst -> [(Text, Value)]
                f i (Catalyst _ s (Just n)) = [(pack $ "smiles_c"  <> (show i), T $ pack s), 
                                               (pack $ "name_c"    <> (show i), T $ pack n)]
                f i (Catalyst _ s _       ) = [(pack $ "smiles_c"  <> (show i), T $ pack s)]
            
            paramListAccelerate = if accelerateList == []
                then []
                else [("t", F $ temperature (head accelerateList)), ("p", F $ pressure (head accelerateList))]
                
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
        pure $ Just (Reaction id (unpack name))

    where 
        cypher = "MATCH (n:Reaction) WHERE ID(n) = {id} RETURN n"
        params = fromList [("id", I id)]

{-
createDemo :: BoltActionT IO ()
createDemo = do
    query "MATCH (n) DETACH DELETE n"
    query cypher
    pure ()
    where 
        cypher = "MERGE (m1:Molecule {name:'3-iodoprop-1-ene',      smiles:'C=CCI',         iupacName:'InChI=1S/C3H5I/c1-2-3-4/h2H,1,3H2'})" <>
                 "MERGE (m2:Molecule {name:'iodide',                smiles:'[I-]',          iupacName:'InChI=1S/HI/h1H/p-1'})" <>
                 "MERGE (m3:Molecule {name:'sodium(1+)',            smiles:'[Na+]',         iupacName:'InChI=1S/Na/q+1'})" <>
                 "MERGE (m4:Molecule {name:'3-bromoprop-1-ene',     smiles:'C=CCBr',        iupacName:'InChI=1S/C3H5Br/c1-2-3-4/h2H,1,3H2'})" <>
                 "MERGE (m5:Molecule {name:'propane',               smiles:'CCC',           iupacName:'InChI=1S/C3H8/c1-3-2/h3H2,1-2H3'})" <>
                 "MERGE (m6:Molecule {name:'prop-1-ene',            smiles:'C=CC',          iupacName:'InChI=1S/C3H6/c1-3-2/h3H,1H2,2H3'})" <>
                 "MERGE (m7:Molecule {name:'methane',               smiles:'C',             iupacName:'InChI=1S/CH4/h1H4'})" <>
                 "MERGE (m8:Molecule {name:'pentane',               smiles:'CCCCC',         iupacName:'InChI=1S/C5H12/c1-3-5-4-2/h3-5H2,1-2H3'})" <>
                 "MERGE (m9:Molecule {name:'2-methylbutane',        smiles:'CCC(C)C',       iupacName:'InChI=1S/C5H12/c1-4-5(2)3/h5H,4H2,1-3H3'})" <>
                 "MERGE (m10:Molecule {name:'oxidane',              smiles:'O',             iupacName:'InChI=1S/H2O/h1H2'})" <>
                 "MERGE (m11:Molecule {name:'molecular hydrogen',   smiles:'[H]',           iupacName:'InChI=1S/H2/h1H'})" <> 
                 "MERGE (m12:Molecule {name:'ethene',               smiles:'C=C',           iupacName:'InChI=1S/C2H4/c1-2/h1-2H2'})" <>
                 "MERGE (m13:Molecule {name:'ethanol',              smiles:'CCO',           iupacName:'InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3'})" <>
                 "MERGE (m14:Molecule {name:'molecular oxygen',     smiles:'O=O',           iupacName:'InChI=1S/O2/c1-2'})" <>
                 "MERGE (m15:Molecule {name:'carbon dioxide',       smiles:'C(=O)=O',       iupacName:'InChI=1S/CO2/c2-1-3'})" <>
                 "MERGE (m16:Molecule {name:'buta-1,3-diene',       smiles:'C=CC=C',        iupacName:'InChI=1S/C4H6/c1-3-4-2/h3-4H,1-2H2'})" <>
                 "MERGE (m17:Molecule {name:'sodium',               smiles:'[Na]',          iupacName:'InChI=1S/Na'})" <>
                 "MERGE (m18:Molecule {name:'sodium methoxide',     smiles:'[Na+].[O-]C',   iupacName:'InChI=1S/CH3O.Na/c1-2;/h1H3;/q-1;+1'})" <>
                 "MERGE (m19:Molecule {name:'methanol',             smiles:'CO',            iupacName:'InChI=1S/CH4O/c1-2/h2H,1H3'})" <>
                 "MERGE (m20:Molecule {name:'acetylene',            smiles:'C#C',           iupacName:'InChI=1S/C2H2/c1-2/h1-2H'})" <>

                 "MERGE (c0:Catalyst {smiles:'',                                name: ''})" <> -- nothing
                 "MERGE (c1:Catalyst {smiles:'[Al](Cl)(Cl)Cl',                  name: 'trichloroalumane'})" <>
                 "MERGE (c2:Catalyst {smiles:'OP(O)(O)=O',                      name: 'phosphoric acid'})" <>
                 "MERGE (c3:Catalyst {smiles:'O=[Zn]',                          name: 'oxozinc'})" <>
                 "MERGE (c4:Catalyst {smiles:'[O-2].[O-2].[O-2].[Al+3].[Al+3]', name: 'aluminum oxide'})" <>

                 "MERGE (r1:Reaction {name: 'displacement reaction'})" <>
                 "MERGE (m2)-[:REAGENT_IN {amount:1.0}]->(r1)" <>
                 "MERGE (m3)-[:REAGENT_IN {amount:1.0}]->(r1)" <>
                 "MERGE (m4)-[:REAGENT_IN {amount:1.0}]->(r1)" <>
                 "MERGE (r1)-[:PRODUCT_FROM {amount:1.0}]->(m4)" <>
                 "MERGE (r1)-[:PRODUCT_FROM {amount:1.0}]->(m1)" <>
        
                 "MERGE (r2:Reaction {name: 'thermal cracking of propane'})" <>
                 "MERGE (m5)-[:REAGENT_IN {amount:1.0}]->(r2)" <>
                 "MERGE (c0)-[:ACCELERATE {temperature:500.0}]->(r2)" <>
                 "MERGE (r2)-[:PRODUCT_FROM {amount:1.0}]->(m6)" <>
                 "MERGE (r2)-[:PRODUCT_FROM {amount:1.0}]->(m11)" <>
        
                 "MERGE (r3:Reaction {name: 'isomerization of n-pentane'})" <>
                 "MERGE (m8)-[:REAGENT_IN {amount:1.0}]->(r3)" <>
                 "MERGE (c1)-[:ACCELERATE {temperature:400.0}]->(r3)" <>
                 "MERGE (r3)-[:PRODUCT_FROM {amount:1.0}]->(m9)" <>

                 "MERGE (r4:Reaction {name: 'hydration of ethanol'})" <>
                 "MERGE (m10)-[:REAGENT_IN {amount:1.0}]->(r4)" <>
                 "MERGE (m12)-[:REAGENT_IN {amount:1.0}]->(r4)" <>
                 "MERGE (c2)-[:ACCELERATE {temperature:300.0, pressure: 7.0 }]->(r4)" <>
                 "MERGE (r4)-[:PRODUCT_FROM {amount:1.0}]->(m13)" <>

                 "MERGE (r5:Reaction {name: 'combustion of ethanol'})" <>
                 "MERGE (m13)-[:REAGENT_IN {amount:1.0}]->(r5)" <>
                 "MERGE (m14)-[:REAGENT_IN {amount:3.0}]->(r5)" <>
                 "MERGE (r5)-[:PRODUCT_FROM {amount:2.0}]->(m15)" <>
                 "MERGE (r5)-[:PRODUCT_FROM {amount:1.0}]->(m10)" <>

                 "MERGE (r6:Reaction {name: 'butadiene synthesis'})" <>
                 "MERGE (m13)-[:REAGENT_IN {amount:2.0}]->(r6)" <>
                 "MERGE (c3)-[:ACCELERATE {temperature:300.0}]->(r6)" <>
                 "MERGE (c4)-[:ACCELERATE {temperature:300.0}]->(r6)" <>
                 "MERGE (r6)-[:PRODUCT_FROM {amount:1.0}]->(m16)" <>
                 "MERGE (r6)-[:PRODUCT_FROM {amount:2.0}]->(m10)" <>
                 "MERGE (r6)-[:PRODUCT_FROM {amount:1.0}]->(m14)" <>

                 "MERGE (r7:Reaction {name: 'sodium methoxide synthesis'})" <>
                 "MERGE (m17)-[:REAGENT_IN {amount:2.0}]->(r6)" <>
                 "MERGE (m19)-[:REAGENT_IN {amount:2.0}]->(r6)" <>
                 "MERGE (r7)-[:PRODUCT_FROM {amount:1.0}]->(m11)" <>
                 "MERGE (r7)-[:PRODUCT_FROM {amount:2.0}]->(m18)" <>

                 "MERGE (r8:Reaction {name: 'dehydrogenation methane'})" <>
                 "MERGE (m7)-[:REAGENT_IN {amount:2.0}]->(r8)" <>
                 "MERGE (c3)-[:ACCELERATE {temperature:1400.0}]->(r8)" <>
                 "MERGE (r8)-[:PRODUCT_FROM {amount:3.0}]->(m11)" <>
                 "MERGE (r8)-[:PRODUCT_FROM {amount:1.0}]->(m20)"
                 
-}

test :: IO () 
test = do
    putStrLn $ unpack (getCypherText name reagentList catalistList accelerateList productList)
    putStrLn $ show (getCypherParams name reagentList catalistList accelerateList productList)
    
    pure ()
    where
        name = "dehydrogenation methane"
        reagentList = [(Molecule 0 "C" "methane", 2.0)]
        catalistList = [] --[Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid")]
        accelerateList = [] -- [Accelerate 1400 100]
        productList = [(Molecule 0 "[H]" "molecular hydrogen", 3.0), 
                       (Molecule 0 "C#C" "acetylene", 1.0)]   



createDemo1 :: BoltActionT IO ()
createDemo1 = do
        query "MATCH (n) DETACH DELETE n"
        
        createReaction 
            "dehydrogenation methane" 
            [(Molecule 0 "C" "methane", 2)]
            [(Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"))]
            [Accelerate 1400 100] 
            [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)] 
    
        createReaction 
            "thermal cracking of propane" 
            [(Molecule 0 "CCC" "propane", 1)]
            []
            [Accelerate 500 0] 
            [(Molecule 0 "C=CC" "prop-1-ene", 1), (Molecule 0 "[H]" "molecular hydrogen", 1)] 
                        
        createReaction 
            "hydration of ethanol" 
            [(Molecule 0 "O" "oxidane", 1), (Molecule 0 "C=C" "ethene", 1)]
            [(Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"))]
            [Accelerate 300 7] 
            [(Molecule 0 "CCO" "ethanol", 1)]
            
        createReaction 
            "combustion of ethanol" 
            [(Molecule 0 "CCO" "ethanol", 1), (Molecule 0 "O=O" "molecular oxygen", 3)]
            []
            [] 
            [(Molecule 0 "C(=O)=O" "carbon dioxide", 2), (Molecule 0 "O" "oxidane", 1)]

        createReaction 
            "butadiene synthesis" 
            [(Molecule 0 "CCO" "ethanol", 2)]
            [(Catalyst 0 "O=[Zn]" (Just "oxozinc")), Catalyst 0 "[O-2].[O-2].[O-2].[Al+3].[Al+3]" (Just "aluminum oxide") ]
            [Accelerate 300 0] 
            [(Molecule 0 "C=CC=C" "buta-1,3-diene", 2.0), (Molecule 0 "O" "oxidane", 2), (Molecule 0 "[H]" "molecular hydrogen", 1)]
        
        createReaction 
            "sodium methoxide synthesis" 
            [(Molecule 0 "[Na]" "sodium", 2), (Molecule 0 "CO" "methanol", 2)]
            []
            [] 
            [(Molecule 0 "[H]" "molecular hydrogen", 1), (Molecule 0 "[Na+].[O-]C" "sodium methoxide", 2)]
        
        createReaction 
            "dehydrogenation methane" 
            [(Molecule 0 "C" "methane", 2)]
            [(Catalyst 0 "O=[Zn]" (Just "oxozinc"))]
            [Accelerate 1400 0] 
            [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)]

        pure ()

                 