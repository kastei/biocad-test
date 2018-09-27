{-# LANGUAGE OverloadedStrings #-}

module Data (createReaction, getReactionById, getShortestPath, createDemo) where 

import Types

import Data.Char
import Data.Map.Strict (fromList, Map)
import Data.Text (Text, pack, unpack)

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe (fromJust, fromMaybe)

import Database.Bolt


createReaction :: String -> [(Molecule, Double)] -> [(Maybe Catalyst, Accelerate)] -> [(Molecule, Double)] -> BoltActionT IO (Maybe Int)
createReaction name reagentList catalistList productList = do            
    result <- head <$> queryP cypher params
    if null result 
    then return Nothing
    else do
        I r_id <- result `at` "id"
        return $ Just r_id
    where
        cypher = cypherText name reagentList catalistList productList
        params = cypherParams name reagentList catalistList productList      
                
        cypherText :: String -> [(Molecule, Double)] -> [(Maybe Catalyst, Accelerate)] -> [(Molecule, Double)] -> Text
        cypherText name reagentList catalistList productList = pack $ 
          "MERGE (r:Reaction {name: {name}})" <> 
          cypherHeadReagent  <> 
          cypherHeadCatalist <> 
          cypherHeadProduct  <> 
          cypherTailReagent  <> 
          cypherTailCatalyst <> 
          cypherTailProduct  <> 
          "RETURN id(r) as id" where            
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
                                
            cypherHeadCatalist = concat $ zipWith f (map show [1..]) catalistList where
                f i (Nothing,               _)        = "MERGE (noCatalist:Catalyst {smiles:'', name:'no catalist'})" 
                f i (Just (Catalyst _ s (Just n)), _) = "MERGE (c" <> i <> ":Catalyst {" <> 
                                                        "smiles:{smiles_c" <> i <> "}," <>
                                                        "name:{name_c" <> i <> "}})"
                f i (Just (Catalyst _ s  _      ), _) = "MERGE (c" <> i <> ":Catalyst {" <> 
                                                        "smiles:{smiles_c" <> i <> "}})"
            
            cypherTailReagent = concat $ zipWith f (map show [1..]) reagentList where
                f i m = "MERGE (mr" <> i <> ")-[:REAGENT_IN {amount: {amount_r" <> i <> "}}]->(r)"

            cypherTailProduct = concat $ zipWith f (map show [1..]) productList where
                f i m = "MERGE (r)-[:PRODUCT_FROM {amount: {amount_p" <> i <> "}}]->(mp" <> i <>")"
    
            cypherTailCatalyst = concat $ zipWith f (map show [1..]) catalistList where
                f i (a, Accelerate p t) = partCat <> partAcc where 
                    partCat = case a of
                        Nothing -> "MERGE (noCatalist)-" 
                        Just _  -> "MERGE (c" <> i <> ")-"
                    partAcc = case (p, t) of
                        (Just _,  Just _)   -> "[:ACCELERATE {temperature:{t" <> i <> "},pressure:{p" <> i <> "}}]->(r)"
                        (Just _,  Nothing)  -> "[:ACCELERATE {temperature:{t" <> i <> "}}]->(r)"
                        (Nothing, Just _)   -> "[:ACCELERATE {pressure:{p" <> i <> "}}]->(r)"
                        (Nothing, Nothing)  -> "[:ACCELERATE]->(r)"

        cypherParams :: String -> [(Molecule, Double)] -> [(Maybe Catalyst, Accelerate)] -> [(Molecule, Double)] -> Map Text Value
        cypherParams name reagentList catalistList productList = fromList $ 
          [("name", T $ pack name)] <> 
          paramListReagent <> 
          paramListProduct <> 
          paramListCatalyst where        
            
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
                f :: String -> (Maybe Catalyst, Accelerate) -> [(Text, Value)]
                
                f i (a, Accelerate temperature pressure) = partCat <> partAcc where 
                    partCat = case a of
                        Nothing                       -> [] 
                        Just (Catalyst _ s (Just n))  -> [(pack $ "smiles_c" <> i, T $ pack s), 
                                                          (pack $ "name_c"   <> i, T $ pack n)]
                        
                        Just (Catalyst _ s Nothing)   -> [(pack $ "smiles_c" <> i, T $ pack s)] 
                    
                    partAcc = case (temperature, pressure) of
                        (Nothing,  Nothing)     -> []
                        (Just t,   Just p)      -> [(pack $ "t" <> i, F $ t), (pack $ "p" <> i, F $ p)]
                        (Nothing,  Just p)      -> [(pack $ "p" <> i, F $ p)]
                        (Just t,   Nothing)     -> [(pack $ "t" <> i, F $ t)]
                


getReactionById :: Int -> BoltActionT IO (Maybe Reaction)
getReactionById id = do 
    result <- head <$> queryP cypher params
    if null result 
    then pure Nothing
    else do
        T name <- result `at` "name"
        L reagents'  <- result `at` "reagents"
        L products'  <- result `at` "products"
        L catalysts' <- result `at` "catalysts"
        
        let reagents = map toMoleculeAndAmount reagents'
        let products = map toMoleculeAndAmount products'
        let catalysts = if (head catalysts') == (L [N (),N (),N (),N (),N ()]) then [] else map toCatalystAndAccelerate catalysts'
    
        return $ Just (Reaction id (unpack name) reagents catalysts products)

    where 
        cypher = "MATCH (reaction) WHERE id(reaction)={id}" <>
                 "MATCH (reaction)<-[amount_in:REAGENT_IN]-(reagent:Molecule) " <>
                 "MATCH (reaction)-[amount_from:PRODUCT_FROM]->(product:Molecule) " <>
                 "OPTIONAL MATCH (reaction)<-[accelerate:ACCELERATE]-(catalyst:Catalyst) " <>
                 "RETURN reaction.name as name," <>
                 "collect(DISTINCT [id(reagent),reagent.smiles,reagent.iupacName,amount_in.amount]) as reagents," <>
                 "collect(DISTINCT [id(product),product.smiles,product.iupacName,amount_from.amount]) as products," <>
                 "collect(DISTINCT [id(catalyst),catalyst.smiles,catalyst.name,accelerate.temperature,accelerate.pressure]) as catalysts"
        params = fromList [("id", I id)]


getShortestPath  :: BoltActionT IO ([(Int, Int)])
getShortestPath = do
    result <- head <$> queryP cypher params
    L path <- result `at` "nodes"
    liftIO $ putStrLn (show path)

    let pairs = toPair (tail path) where
        toPair :: [Value] -> [(Value, Value)]
        toPair [] = []
        toPair (a:b:xs) = [(a,b)] ++ (toPair xs)

    reactions <- traverse toReaction (map fst pairs)
    molecules <- traverse toMolecule (map snd pairs)

    liftIO $ putStrLn (show molecules)
    liftIO $ putStrLn (show reactions)
    liftIO $ putStrLn (show $ zipWith (\r m -> (r_id r, m_id m) ) reactions molecules)

    return $ zipWith (\r m -> (r_id r, m_id m) ) reactions molecules

    where
        cypher = "MATCH (start:Molecule), (end:Molecule), path=shortestPath((start)-[*]->(end))" <> 
                 "WHERE start.smiles={start_smiles} and end.smiles={end_smiles}" <> 
                 "RETURN nodes(path) as nodes"
        params = fromList [("start_smiles", T "C=C"), ("end_smiles", T "O")]      
    

createDemo :: BoltActionT IO ()
createDemo = do
        query "MATCH (n) DETACH DELETE n"
        
        id <- createReaction 
            "1. dehydrogenation methane 1" 
            [(Molecule 0 "C" "methane", 2)]
            [(Just $ Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"), Accelerate (Just 1400) (Just 100))]
            [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)]
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))

        id <- createReaction 
            "2. dehydrogenation methane 2" 
            [(Molecule 0 "C" "methane", 2)]
            [(Just $ Catalyst 0 "O=[Zn]" (Just "oxozinc"), Accelerate (Just 1400) Nothing)]
            [(Molecule 0 "[H]" "molecular hydrogen", 3), (Molecule 0 "C#C" "acetylene", 1)]
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))
                
        id <- createReaction 
            "3. thermal cracking of propane" 
            [(Molecule 0 "CCC" "propane", 1)]
            [(Nothing, Accelerate (Just 500) Nothing)]
            [(Molecule 0 "C=CC" "prop-1-ene", 1), (Molecule 0 "[H]" "molecular hydrogen", 1)] 
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))
    
        id <- createReaction 
            "4. hydration of ethanol" 
            [(Molecule 0 "O" "oxidane", 1), (Molecule 0 "C=C" "ethene", 1)]
            [(Just $ Catalyst 0 "OP(O)(O)=O" (Just "phosphoric acid"), Accelerate (Just 300) (Just 7))]
            [(Molecule 0 "CCO" "ethanol", 1)]

        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))

        id <- createReaction 
            "5. combustion of ethanol" 
            [(Molecule 0 "CCO" "ethanol", 1), (Molecule 0 "O=O" "molecular oxygen", 3)]
            []
            [(Molecule 0 "C(=O)=O" "carbon dioxide", 2), (Molecule 0 "O" "oxidane", 1)]
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))
            
        id <- createReaction 
            "6. butadiene synthesis" 
            [(Molecule 0 "CCO" "ethanol", 2)]
            [(Just $ Catalyst 0 "O=[Zn]" (Just "oxozinc"), Accelerate (Just 300) Nothing), 
              (Just $ Catalyst 0 "[O-2].[O-2].[O-2].[Al+3].[Al+3]" (Just "aluminum oxide"), Accelerate (Just 300) Nothing)]
            [(Molecule 0 "C=CC=C" "buta-1,3-diene", 2.0), (Molecule 0 "O" "oxidane", 2), (Molecule 0 "[H]" "molecular hydrogen", 1)]
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))
            
        id <- createReaction 
            "7. sodium methoxide synthesis" 
            [(Molecule 0 "[Na]" "sodium", 2), (Molecule 0 "CO" "methanol", 2)]
            []
            [(Molecule 0 "[H]" "molecular hydrogen", 1), (Molecule 0 "[Na+].[O-]C" "sodium methoxide", 2)]
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))
    
        id <- createReaction 
            "8. trimerization of acetylene" 
            [(Molecule 0 "C#C" "acetylene", 3)]
            [(Just $ Catalyst 0 "[C]" (Just "carbon"), Accelerate (Just 650) Nothing )]
            [(Molecule 0 "c1ccccc1" "benzene ", 1)]
        
        reaction <- getReactionById $(fromJust id)
        liftIO $ putStrLn (fromMaybe "" (fmap show reaction))
    
        return ()